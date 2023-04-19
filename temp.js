var Module = globalThis.Module || {};

var ENVIRONMENT_IS_SHELL = typeof read == "function";

if (ENVIRONMENT_IS_SHELL) {
 Module["wasm"] = read("temp.wasm", "binary");
}

var out = text => console.log(text);

var err = text => console.error(text);

function ready() {}

var UTF8Decoder = typeof TextDecoder != "undefined" ? new TextDecoder("utf8") : undefined;

function UTF8ArrayToString(heapOrArray, idx, maxBytesToRead) {
 var endIdx = idx + maxBytesToRead;
 var endPtr = idx;
 while (heapOrArray[endPtr] && !(endPtr >= endIdx)) ++endPtr;
 if (endPtr - idx > 16 && heapOrArray.buffer && UTF8Decoder) {
  return UTF8Decoder.decode(heapOrArray.subarray(idx, endPtr));
 }
 var str = "";
 while (idx < endPtr) {
  var u0 = heapOrArray[idx++];
  if (!(u0 & 128)) {
   str += String.fromCharCode(u0);
   continue;
  }
  var u1 = heapOrArray[idx++] & 63;
  if ((u0 & 224) == 192) {
   str += String.fromCharCode((u0 & 31) << 6 | u1);
   continue;
  }
  var u2 = heapOrArray[idx++] & 63;
  if ((u0 & 240) == 224) {
   u0 = (u0 & 15) << 12 | u1 << 6 | u2;
  } else {
   u0 = (u0 & 7) << 18 | u1 << 12 | u2 << 6 | heapOrArray[idx++] & 63;
  }
  if (u0 < 65536) {
   str += String.fromCharCode(u0);
  } else {
   var ch = u0 - 65536;
   str += String.fromCharCode(55296 | ch >> 10, 56320 | ch & 1023);
  }
 }
 return str;
}

function UTF8ToString(ptr, maxBytesToRead) {
 return ptr ? UTF8ArrayToString(HEAPU8, ptr, maxBytesToRead) : "";
}

function stringToUTF8Array(str, heap, outIdx, maxBytesToWrite) {
 if (!(maxBytesToWrite > 0)) return 0;
 var startIdx = outIdx;
 var endIdx = outIdx + maxBytesToWrite - 1;
 for (var i = 0; i < str.length; ++i) {
  var u = str.charCodeAt(i);
  if (u >= 55296 && u <= 57343) {
   var u1 = str.charCodeAt(++i);
   u = 65536 + ((u & 1023) << 10) | u1 & 1023;
  }
  if (u <= 127) {
   if (outIdx >= endIdx) break;
   heap[outIdx++] = u;
  } else if (u <= 2047) {
   if (outIdx + 1 >= endIdx) break;
   heap[outIdx++] = 192 | u >> 6;
   heap[outIdx++] = 128 | u & 63;
  } else if (u <= 65535) {
   if (outIdx + 2 >= endIdx) break;
   heap[outIdx++] = 224 | u >> 12;
   heap[outIdx++] = 128 | u >> 6 & 63;
   heap[outIdx++] = 128 | u & 63;
  } else {
   if (outIdx + 3 >= endIdx) break;
   heap[outIdx++] = 240 | u >> 18;
   heap[outIdx++] = 128 | u >> 12 & 63;
   heap[outIdx++] = 128 | u >> 6 & 63;
   heap[outIdx++] = 128 | u & 63;
  }
 }
 heap[outIdx] = 0;
 return outIdx - startIdx;
}

var HEAP8, HEAP16, HEAP32, HEAPU8, HEAPU16, HEAPU32, HEAPF32, HEAPF64, wasmMemory, wasmTable;

function updateMemoryViews() {
 var b = wasmMemory.buffer;
 HEAP8 = new Int8Array(b);
 HEAP16 = new Int16Array(b);
 HEAP32 = new Int32Array(b);
 HEAPU8 = new Uint8Array(b);
 HEAPU16 = new Uint16Array(b);
 HEAPU32 = new Uint32Array(b);
 HEAPF32 = new Float32Array(b);
 HEAPF64 = new Float64Array(b);
}

var exceptionCaught = [];

function exception_addRef(info) {
 info.add_ref();
}

var uncaughtExceptionCount = 0;

function ___cxa_begin_catch(ptr) {
 var info = new ExceptionInfo(ptr);
 if (!info.get_caught()) {
  info.set_caught(true);
  uncaughtExceptionCount--;
 }
 info.set_rethrown(false);
 exceptionCaught.push(info);
 exception_addRef(info);
 return info.get_exception_ptr();
}

var exceptionLast = 0;

var wasmTableMirror = [];

function getWasmTableEntry(funcPtr) {
 var func = wasmTableMirror[funcPtr];
 if (!func) {
  if (funcPtr >= wasmTableMirror.length) wasmTableMirror.length = funcPtr + 1;
  wasmTableMirror[funcPtr] = func = wasmTable.get(funcPtr);
 }
 return func;
}

function exception_decRef(info) {
 if (info.release_ref() && !info.get_rethrown()) {
  var destructor = info.get_destructor();
  if (destructor) {
   getWasmTableEntry(destructor)(info.excPtr);
  }
  ___cxa_free_exception(info.excPtr);
 }
}

function ___cxa_end_catch() {
 _setThrew(0);
 var info = exceptionCaught.pop();
 exception_decRef(info);
 exceptionLast = 0;
}

function ExceptionInfo(excPtr) {
 this.excPtr = excPtr;
 this.ptr = excPtr - 24;
 this.set_type = function(type) {
  HEAPU32[this.ptr + 4 >> 2] = type;
 };
 this.get_type = function() {
  return HEAPU32[this.ptr + 4 >> 2];
 };
 this.set_destructor = function(destructor) {
  HEAPU32[this.ptr + 8 >> 2] = destructor;
 };
 this.get_destructor = function() {
  return HEAPU32[this.ptr + 8 >> 2];
 };
 this.set_refcount = function(refcount) {
  HEAP32[this.ptr >> 2] = refcount;
 };
 this.set_caught = function(caught) {
  caught = caught ? 1 : 0;
  HEAP8[this.ptr + 12 >> 0] = caught;
 };
 this.get_caught = function() {
  return HEAP8[this.ptr + 12 >> 0] != 0;
 };
 this.set_rethrown = function(rethrown) {
  rethrown = rethrown ? 1 : 0;
  HEAP8[this.ptr + 13 >> 0] = rethrown;
 };
 this.get_rethrown = function() {
  return HEAP8[this.ptr + 13 >> 0] != 0;
 };
 this.init = function(type, destructor) {
  this.set_adjusted_ptr(0);
  this.set_type(type);
  this.set_destructor(destructor);
  this.set_refcount(0);
  this.set_caught(false);
  this.set_rethrown(false);
 };
 this.add_ref = function() {
  var value = HEAP32[this.ptr >> 2];
  HEAP32[this.ptr >> 2] = value + 1;
 };
 this.release_ref = function() {
  var prev = HEAP32[this.ptr >> 2];
  HEAP32[this.ptr >> 2] = prev - 1;
  return prev === 1;
 };
 this.set_adjusted_ptr = function(adjustedPtr) {
  HEAPU32[this.ptr + 16 >> 2] = adjustedPtr;
 };
 this.get_adjusted_ptr = function() {
  return HEAPU32[this.ptr + 16 >> 2];
 };
 this.get_exception_ptr = function() {
  var isPointer = ___cxa_is_pointer_type(this.get_type());
  if (isPointer) {
   return HEAPU32[this.excPtr >> 2];
  }
  var adjusted = this.get_adjusted_ptr();
  if (adjusted !== 0) return adjusted;
  return this.excPtr;
 };
}

function ___resumeException(ptr) {
 if (!exceptionLast) {
  exceptionLast = ptr;
 }
 throw ptr;
}

function ___cxa_find_matching_catch() {
 var thrown = exceptionLast;
 if (!thrown) {
  setTempRet0(0);
  return 0;
 }
 var info = new ExceptionInfo(thrown);
 info.set_adjusted_ptr(thrown);
 var thrownType = info.get_type();
 if (!thrownType) {
  setTempRet0(0);
  return thrown;
 }
 for (var i = 0; i < arguments.length; i++) {
  var caughtType = arguments[i];
  if (caughtType === 0 || caughtType === thrownType) {
   break;
  }
  var adjusted_ptr_addr = info.ptr + 16;
  if (___cxa_can_catch(caughtType, thrownType, adjusted_ptr_addr)) {
   setTempRet0(caughtType);
   return thrown;
  }
 }
 setTempRet0(thrownType);
 return thrown;
}

var ___cxa_find_matching_catch_2 = ___cxa_find_matching_catch;

var ___cxa_find_matching_catch_3 = ___cxa_find_matching_catch;

function ___cxa_throw(ptr, type, destructor) {
 var info = new ExceptionInfo(ptr);
 info.init(type, destructor);
 exceptionLast = ptr;
 uncaughtExceptionCount++;
 throw ptr;
}

function _emscripten_memcpy_big(dest, src, num) {
 HEAPU8.copyWithin(dest, src, src + num);
}

function getHeapMax() {
 return 2147483648;
}

function emscripten_realloc_buffer(size) {
 var b = wasmMemory.buffer;
 try {
  wasmMemory.grow(size - b.byteLength + 65535 >>> 16);
  updateMemoryViews();
  return 1;
 } catch (e) {}
}

function _emscripten_resize_heap(requestedSize) {
 var oldSize = HEAPU8.length;
 requestedSize = requestedSize >>> 0;
 var maxHeapSize = getHeapMax();
 if (requestedSize > maxHeapSize) {
  return false;
 }
 let alignUp = (x, multiple) => x + (multiple - x % multiple) % multiple;
 for (var cutDown = 1; cutDown <= 4; cutDown *= 2) {
  var overGrownHeapSize = oldSize * (1 + .2 / cutDown);
  overGrownHeapSize = Math.min(overGrownHeapSize, requestedSize + 100663296);
  var newSize = Math.min(maxHeapSize, alignUp(Math.max(requestedSize, overGrownHeapSize), 65536));
  var replacement = emscripten_realloc_buffer(newSize);
  if (replacement) {
   return true;
  }
 }
 return false;
}

var printCharBuffers = [ null, [], [] ];

function printChar(stream, curr) {
 var buffer = printCharBuffers[stream];
 if (curr === 0 || curr === 10) {
  (stream === 1 ? out : err)(UTF8ArrayToString(buffer, 0));
  buffer.length = 0;
 } else {
  buffer.push(curr);
 }
}

var SYSCALLS = {
 varargs: undefined,
 get: function() {
  SYSCALLS.varargs += 4;
  var ret = HEAP32[SYSCALLS.varargs - 4 >> 2];
  return ret;
 },
 getStr: function(ptr) {
  var ret = UTF8ToString(ptr);
  return ret;
 }
};

function _fd_write(fd, iov, iovcnt, pnum) {
 var num = 0;
 for (var i = 0; i < iovcnt; i++) {
  var ptr = HEAPU32[iov >> 2];
  var len = HEAPU32[iov + 4 >> 2];
  iov += 8;
  for (var j = 0; j < len; j++) {
   printChar(fd, HEAPU8[ptr + j]);
  }
  num += len;
 }
 HEAPU32[pnum >> 2] = num;
 return 0;
}

function _llvm_eh_typeid_for(type) {
 return type;
}

var wasmImports = {
 "__cxa_begin_catch": ___cxa_begin_catch,
 "__cxa_end_catch": ___cxa_end_catch,
 "__cxa_find_matching_catch_2": ___cxa_find_matching_catch_2,
 "__cxa_find_matching_catch_3": ___cxa_find_matching_catch_3,
 "__cxa_throw": ___cxa_throw,
 "__resumeException": ___resumeException,
 "emscripten_memcpy_big": _emscripten_memcpy_big,
 "emscripten_resize_heap": _emscripten_resize_heap,
 "fd_write": _fd_write,
 "invoke_iii": invoke_iii,
 "invoke_viii": invoke_viii,
 "llvm_eh_typeid_for": _llvm_eh_typeid_for
};

function invoke_iii(index, a1, a2) {
 var sp = stackSave();
 try {
  return getWasmTableEntry(index)(a1, a2);
 } catch (e) {
  stackRestore(sp);
  if (e !== e + 0) throw e;
  _setThrew(1, 0);
 }
}

function invoke_viii(index, a1, a2, a3) {
 var sp = stackSave();
 try {
  getWasmTableEntry(index)(a1, a2, a3);
 } catch (e) {
  stackRestore(sp);
  if (e !== e + 0) throw e;
  _setThrew(1, 0);
 }
}

function initRuntime(asm) {
 asm["__wasm_call_ctors"]();
}

var imports = {
 "env": wasmImports,
 "wasi_snapshot_preview1": wasmImports
};

var __Z6foobarv, getTempRet0, ___cxa_free_exception, __Z6_startv, _setThrew, setTempRet0, stackSave, stackRestore, stackAlloc, ___cxa_can_catch, ___cxa_is_pointer_type, dynCall_jiji;

WebAssembly.instantiate(Module["wasm"], imports).then(function(output) {
 var asm = output.instance.exports;
 __Z6foobarv = asm["_Z6foobarv"];
 getTempRet0 = asm["getTempRet0"];
 ___cxa_free_exception = asm["__cxa_free_exception"];
 __Z6_startv = asm["_Z6_startv"];
 _setThrew = asm["setThrew"];
 setTempRet0 = asm["setTempRet0"];
 stackSave = asm["stackSave"];
 stackRestore = asm["stackRestore"];
 stackAlloc = asm["stackAlloc"];
 ___cxa_can_catch = asm["__cxa_can_catch"];
 ___cxa_is_pointer_type = asm["__cxa_is_pointer_type"];
 dynCall_jiji = asm["dynCall_jiji"];
 wasmTable = asm["__indirect_function_table"];
 wasmMemory = asm["memory"];
 updateMemoryViews();
 initRuntime(asm);
 ready();
});
