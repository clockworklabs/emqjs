import { getQuickJS } from 'quickjs-emscripten';
import { readFile } from 'fs/promises';
import { inspect } from 'util';

const Module_wasm = await WebAssembly.compile(await readFile('temp.wasm'));

const QuickJS = await getQuickJS();
const vm = QuickJS.newContext();

/** @type {import('quickjs-emscripten').QuickJSHandle[]?} */
let argHandles;

let wasmMemory;

/**
 * @param {import("quickjs-emscripten").QuickJSHandle} vmFunction
 */
function vm2func(vmFunction, convertValue = true) {
  return (/** @type {any[]} */ ...args) => {
    // Copy all the Wasm memory to JS so that modifications from Wasm are reflected in JS.
    // TODO: handle growth.
    vm.getProp(vm.global, 'HEAPU8').consume(vmWasmMemoryView => {
      let wasmMemoryView = new Uint8Array(wasmMemory.buffer);
      let length = vm.getProp(vmWasmMemoryView, 'length').consume(vm.getNumber);
      for (let i = 0; i < length; i++) {
        vm.setProp(vmWasmMemoryView, i, vm.newNumber(wasmMemoryView[i]));
      }
    });
    let value = unwrapResult(
      vm.callFunction(vmFunction, vm.null, ...args.map(js2vm)),
      convertValue
    );
    // Copy all the Wasm memory back so that modifications from JS are reflected in Wasm.
    vm.getProp(vm.global, 'HEAPU8').consume(vmWasmMemoryView => {
      let wasmMemoryView = new Uint8Array(wasmMemory.buffer);
      let length = vm.getProp(vmWasmMemoryView, 'length').consume(vm.getNumber);
      for (let i = 0; i < length; i++) {
        wasmMemoryView[i] = vm
          .getProp(vmWasmMemoryView, i)
          .consume(vm.getNumber);
      }
    });
    return value;
  };
}

/**
 * @param {any} jsValue
 * @returns {import('quickjs-emscripten').QuickJSHandle}
 */
function js2vm(jsValue) {
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
      if (
        jsValue instanceof WebAssembly.Memory ||
        jsValue instanceof WebAssembly.Table
      ) {
        let vmObject = vm.newObject();
        for (let [name, desc] of Object.entries(
          Object.getOwnPropertyDescriptors(Object.getPrototypeOf(jsValue))
        )) {
          if (name === 'constructor' || typeof name === 'symbol') {
            continue;
          }
          let vmDesc = {
            configurable: desc.configurable,
            enumerable: desc.enumerable
          };
          if ('get' in desc) {
            vmDesc.get = () => {
              let value = desc.get.call(jsValue);
              if (typeof value === 'function') {
                value = value.bind(jsValue);
              }
              return js2vm(value);
            };
          }
          if ('set' in desc) {
            vmDesc.set = value => js2vm(desc.set.call(jsValue, vm.dump(value)));
          }
          if ('value' in desc) {
            let value = desc.value;
            if (typeof value === 'function') {
              value = value.bind(jsValue);
            }
            vmDesc.value = js2vm(value);
          }
          vm.defineProp(vmObject, name, vmDesc);
        }
        return vmObject;
      }
      if (jsValue instanceof ArrayBuffer) {
        // This is pretty bad but sharing ArrayBuffer without copying is not possible:
        // https://github.com/justjake/quickjs-emscripten/issues/68
        return unwrapResult(
          vm.evalCode(`new ArrayBuffer(${jsValue.byteLength})`),
          false
        );
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
    error: console.error
  },
  Module: {
    // dummy value, we only have one module and we know which one it is
    wasm: {}
  },
  WebAssembly: {
    instantiate: (
      /** @type {unknown} */ _vmModule_wasm,
      /** @type {WebAssembly.Imports} */ imports
    ) => {
      return vm
        .getProp(vm.global, 'Object')
        .consume(vmObject => vm.getProp(vmObject, 'keys'))
        .consume(async vmObject_keys_handle => {
          let vmObject_keys = (
            /** @type {import("quickjs-emscripten").QuickJSHandle} */ handle
          ) =>
            unwrapResult(
              vm.callFunction(vmObject_keys_handle, vm.null, handle)
            );

          // vm.dump doesn't take care of functions, so patch them in manually
          let vmImports = argHandles[1];
          for (let namespace in imports) {
            let importsNS = imports[namespace];
            vm.getProp(vmImports, namespace).consume(vmImportsNS => {
              let vmImportsNSKeys = vmObject_keys(vmImportsNS);
              for (let key of vmImportsNSKeys) {
                let vmImport = vm.getProp(vmImportsNS, key);
                if (vm.typeof(vmImport) === 'function') {
                  importsNS[key] = vm2func(vmImport, false);
                } else {
                  vmImport.dispose();
                }
              }
            });
          }

          let instance = await WebAssembly.instantiate(Module_wasm, imports);
          wasmMemory = instance.exports.memory;
          return {
            instance: {
              exports: instance.exports
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
    let { value } = result;
    return convertValue ? value.consume(vm.dump) : value;
  }
  let error = result.error.consume(vm.dump);
  if (typeof error === 'object' && 'stack' in error) {
    let err = new (globalThis[error.name] || Error)(error.message);
    err.stack = `${error.name}: ${error.message}\n${error.stack}${err.stack
      .split('\n')
      .slice(1)
      .join('\n')}`;
    error = err;
  }
  throw error;
}

const fileName = 'temp.js';
const code = await readFile(fileName, 'utf8');
unwrapResult(vm.evalCode(code, fileName), false).dispose();
