import { getQuickJS } from 'quickjs-emscripten';
import { readFile } from 'fs/promises';
import { inspect } from 'util';

const Module_wasm = await WebAssembly.compile(await readFile('temp.wasm'));

const QuickJS = await getQuickJS();
const vm = QuickJS.newContext();

/** @type {import('quickjs-emscripten').QuickJSHandle[]?} */
let argHandles;

/** @type {WeakMap<object, import('quickjs-emscripten').QuickJSHandle>} */
let js2vmCache = new WeakMap();

/**
 * @param {import("quickjs-emscripten").QuickJSHandle} vmFunction
 */
function vm2func(vmFunction) {
  return (/** @type {any[]} */ ...args) => unwrapResult(vm.callFunction(vmFunction, vm.null, ...args.map(js2vm)));
}

/**
 * @param {any} jsValue
 * @returns {import('quickjs-emscripten').QuickJSHandle}
 */
function js2vm_(jsValue) {
  switch (typeof jsValue) {
    case 'undefined':
      return vm.undefined;
    case 'boolean':
      return jsValue ? vm.true : vm.false;
    case 'number':
      return vm.newNumber(jsValue);
    case 'string':
      return vm.newString(jsValue);
    case 'object':
      if (jsValue === null) {
        return vm.null;
      }
      if (jsValue instanceof Promise) {
        let promise = vm.newPromise(jsValue.then(js2vm));
        promise.settled.then(() => vm.runtime.executePendingJobs());
        return promise.handle;
      }
      if (jsValue instanceof Error) {
        return vm.newError(jsValue);
      }
      if (Array.isArray(jsValue)) {
        let arr = vm.newArray();
        for (let i = 0; i < jsValue.length; i++) {
          js2vm(jsValue[i]).consume(vmItem => vm.setProp(arr, i, vmItem));
        }
        return arr;
      }
      if ((jsValue instanceof WebAssembly.Memory) || (jsValue instanceof WebAssembly.Table)) {
        let vmObject = vm.newObject();
        for (let [name, desc] of Object.entries(Object.getOwnPropertyDescriptors(Object.getPrototypeOf(jsValue)))) {
          if (name === 'constructor' || typeof name === 'symbol') {
            continue;
          }
          vm.defineProp(vmObject, name, {
            configurable: desc.configurable,
            enumerable: desc.enumerable,
            get: () => js2vm(desc.get.call(jsValue)),
            set: desc.set ? (value) => js2vm(desc.set.call(jsValue, vm.dump(value))) : undefined,
            value: desc.value !== undefined ? js2vm(desc.value) : undefined,
          });
        }
        return vmObject;
      }
      if (jsValue instanceof ArrayBuffer) {
        // This is pretty bad but sharing ArrayBuffer without copying is not possible:
        // https://github.com/justjake/quickjs-emscripten/issues/68
        let byteArray = new Uint8Array(jsValue);
        return unwrapResult(vm.evalCode(`new Uint8Array(${jsValue.byteLength})`), false).consume(vmByteArray => {
          for (let i = 0; i < byteArray.length; i++) {
            vm.setProp(vmByteArray, i, vm.newNumber(byteArray[i]));
          }
          return vm.getProp(vmByteArray, 'buffer');
        });
      }
      if (jsValue.constructor === Object || jsValue.constructor === undefined) {
        let obj = vm.newObject();
        for (let [key, value] of Object.entries(jsValue)) {
          js2vm(value).consume(vmValue => vm.setProp(obj, key, vmValue));
        }
        return obj;
      }
      throw new Error(`Unsupported JS object ${inspect(jsValue)}`);
    case 'function':
      return vm.newFunction(jsValue.name, (...args) => {
        argHandles = args;
        try {
          return js2vm(jsValue(...args.map(vm.dump)));
        } finally {
          argHandles = undefined;
        }
      });
    default:
      throw new Error(`Unsupported JS type ${typeof jsValue}`);
  }
}

/**
 * @param {any} jsValue
 * @returns {import('quickjs-emscripten').QuickJSHandle}
 */
function js2vm(jsValue) {
  let isCacheable = typeof jsValue === 'object' && jsValue !== null || typeof jsValue === 'function';
  if (isCacheable) {
    let cached = js2vmCache.get(jsValue);
    if (cached) {
      return cached.dup();
    }
  }
  let vmValue = js2vm_(jsValue);
  if (isCacheable) {
    js2vmCache.set(jsValue, vmValue.dup());
  }
  return vmValue;
}

/**
 * @param {Record<string, any>} obj
 */
function setGlobals(obj) {
  for (const [name, value] of Object.entries(obj)) {
    js2vm(value).consume(vmValue => vm.setProp(vm.global, name, vmValue));
  }
}

setGlobals({
  console: {
    log: console.log,
    warn: console.warn,
    error: console.error,
  },
  Module: {
    // dummy value, we only have one module and we know which one it is
    wasm: {},
  },
  WebAssembly: {
    instantiate: (/** @type {unknown} */ _vmModule_wasm, /** @type {WebAssembly.Imports} */ imports) => {
      return vm.getProp(vm.global, 'Object').consume(vmObject => vm.getProp(vmObject, 'keys')).consume(async vmObject_keys_handle => {
        let vmObject_keys = (/** @type {import("quickjs-emscripten").QuickJSHandle} */ handle) => unwrapResult(vm.callFunction(vmObject_keys_handle, vm.null, handle));

        // vm.dump doesn't take care of functions, so patch them in manually
        let vmImports = argHandles[1];
        for (let namespace in imports) {
          let importsNS = imports[namespace];
          vm.getProp(vmImports, namespace).consume(vmImportsNS => {
            let vmImportsNSKeys = vmObject_keys(vmImportsNS);
            for (let key of vmImportsNSKeys) {
              let vmImport = vm.getProp(vmImportsNS, key);
              if (vm.typeof(vmImport) === 'function') {
                importsNS[key] = vm2func(vmImport);
              } else {
                vmImport.dispose();
              }
            }
          });
        }

        let instance = await WebAssembly.instantiate(Module_wasm, imports);
        return {
          instance: {
            exports: instance.exports,
          }
        };
      });
    }
  }
});

/**
 * @param {import('quickjs-emscripten').VmCallResult<import('quickjs-emscripten').QuickJSHandle>} result
 */
function unwrapResult(result, convertValue = true) {
  if ('value' in result) {
    let value = result.value;
    if (convertValue) {
      value = value.consume(vm.dump);
    }
    return value;
  } else {
    let error = result.error.consume(vm.dump);
    if (typeof error === 'object' && 'stack' in error) {
      console.log(error);
      let err = new (globalThis[error.name] || Error)(error.message);
      err.stack = `${error.name}: ${error.message}\n${
        error.stack
      }${err.stack.split('\n').slice(1).join('\n')}`;
      error = err;
    }
    throw error;
  }
}

const fileName = 'temp.js';
const code = await readFile(fileName, 'utf8');
unwrapResult(vm.evalCode(code, fileName));
