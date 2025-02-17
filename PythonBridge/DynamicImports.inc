CodePointer(Py_DebugFlag)               := Import('Py_DebugFlag');
CodePointer(Py_VerboseFlag)             := Import('Py_VerboseFlag');
CodePointer(Py_InteractiveFlag)         := Import('Py_InteractiveFlag');
CodePointer(Py_OptimizeFlag)            := Import('Py_OptimizeFlag');
CodePointer(Py_NoSiteFlag)              := Import('Py_NoSiteFlag');
CodePointer(Py_FrozenFlag)              := Import('Py_FrozenFlag');
CodePointer(Py_IgnoreEnvironmentFlag)   := Import('Py_IgnoreEnvironmentFlag');
// https://docs.python.org/3.3/c-api/bool.html
CodePointer(Py_None)                    := Import('_Py_NoneStruct');
CodePointer(Py_False)                   := Import('_Py_FalseStruct');
CodePointer(Py_True)                    := Import('_Py_TrueStruct');
CodePointer(Py_Ellipsis)                := Import('_Py_EllipsisObject');
CodePointer(Py_NotImplemented)          := Import('_Py_NotImplementedStruct');
CodePointer(PyImport_FrozenModules)     := Import('PyImport_FrozenModules');
CodePointer(PyExc_AttributeError)       := Import('PyExc_AttributeError');
CodePointer(PyExc_EOFError)             := Import('PyExc_EOFError');
CodePointer(PyExc_IOError)              := Import('PyExc_IOError');
CodePointer(PyExc_ImportError)          := Import('PyExc_ImportError');
CodePointer(PyExc_IndexError)           := Import('PyExc_IndexError');
CodePointer(PyExc_KeyError)             := Import('PyExc_KeyError');
CodePointer(PyExc_KeyboardInterrupt)    := Import('PyExc_KeyboardInterrupt');
CodePointer(PyExc_MemoryError)          := Import('PyExc_MemoryError');
CodePointer(PyExc_NameError)            := Import('PyExc_NameError');
CodePointer(PyExc_OverflowError)        := Import('PyExc_OverflowError');
CodePointer(PyExc_RuntimeError)         := Import('PyExc_RuntimeError');
CodePointer(PyExc_SyntaxError)          := Import('PyExc_SyntaxError');
CodePointer(PyExc_SystemError)          := Import('PyExc_SystemError');
CodePointer(PyExc_SystemExit)           := Import('PyExc_SystemExit');
CodePointer(PyExc_TypeError)            := Import('PyExc_TypeError');
CodePointer(PyExc_ValueError)           := Import('PyExc_ValueError');
CodePointer(PyExc_ZeroDivisionError)    := Import('PyExc_ZeroDivisionError');
CodePointer(PyExc_ArithmeticError)      := Import('PyExc_ArithmeticError');
CodePointer(PyExc_Exception)            := Import('PyExc_Exception');
CodePointer(PyExc_FloatingPointError)   := Import('PyExc_FloatingPointError');
CodePointer(PyExc_LookupError)          := Import('PyExc_LookupError');
CodePointer(PyExc_AssertionError)       := Import('PyExc_AssertionError');
CodePointer(PyExc_EnvironmentError)     := Import('PyExc_EnvironmentError');
CodePointer(PyExc_IndentationError)     := Import('PyExc_IndentationError');
CodePointer(PyExc_NotImplementedError)  := Import('PyExc_NotImplementedError');
CodePointer(PyExc_OSError)              := Import('PyExc_OSError');
CodePointer(PyExc_TabError)             := Import('PyExc_TabError');
CodePointer(PyExc_UnboundLocalError)    := Import('PyExc_UnboundLocalError');
CodePointer(PyExc_UnicodeError)         := Import('PyExc_UnicodeError');
{$IFDEF windows}
CodePointer(PyExc_WindowsError)       := Import('PyExc_WindowsError');
{$ENDIF}
CodePointer(PyExc_Warning)              := Import('PyExc_Warning');
CodePointer(PyExc_DeprecationWarning)   := Import('PyExc_DeprecationWarning');
CodePointer(PyExc_RuntimeWarning)       := Import('PyExc_RuntimeWarning');
CodePointer(PyExc_SyntaxWarning)        := Import('PyExc_SyntaxWarning');
CodePointer(PyExc_UserWarning)          := Import('PyExc_UserWarning');
CodePointer(PyExc_ReferenceError)       := Import('PyExc_ReferenceError');
CodePointer(PyExc_StopIteration)        := Import('PyExc_StopIteration');
CodePointer(PyExc_FutureWarning)        := Import('PyExc_FutureWarning');
CodePointer(PyExc_PendingDeprecationWarning):= Import('PyExc_PendingDeprecationWarning');
CodePointer(PyExc_UnicodeDecodeError)   := Import('PyExc_UnicodeDecodeError');
CodePointer(PyExc_UnicodeEncodeError)   := Import('PyExc_UnicodeEncodeError');
CodePointer(PyExc_UnicodeTranslateError):= Import('PyExc_UnicodeTranslateError');
CodePointer(PyType_Type)                := Import('PyType_Type');
CodePointer(PyCFunction_Type)           := Import('PyCFunction_Type');
CodePointer(PyCode_Type)                := Import('PyCode_Type');
CodePointer(PyComplex_Type)             := Import('PyComplex_Type');
CodePointer(PyDict_Type)                := Import('PyDict_Type');
CodePointer(PyFloat_Type)               := Import('PyFloat_Type');
CodePointer(PyFrame_Type)               := Import('PyFrame_Type');
CodePointer(PyFunction_Type)            := Import('PyFunction_Type');
CodePointer(PyList_Type)                := Import('PyList_Type');
CodePointer(PyLong_Type)                := Import('PyLong_Type');
CodePointer(PyMethod_Type)              := Import('PyMethod_Type');
CodePointer(PyModule_Type)              := Import('PyModule_Type');
CodePointer(PyObject_Type)              := Import('PyObject_Type');
CodePointer(PyRange_Type)               := Import('PyRange_Type');
CodePointer(PySlice_Type)               := Import('PySlice_Type');
CodePointer(PyTuple_Type)               := Import('PyTuple_Type');
CodePointer(PyUnicode_Type)             := Import('PyUnicode_Type');
CodePointer(PyBaseObject_Type)          := Import('PyBaseObject_Type');
CodePointer(PyCallIter_Type)            := Import('PyCallIter_Type');
CodePointer(PyCell_Type)                := Import('PyCell_Type');
CodePointer(PyClassMethod_Type)         := Import('PyClassMethod_Type');
CodePointer(PyProperty_Type)            := Import('PyProperty_Type');
CodePointer(PySeqIter_Type)             := Import('PySeqIter_Type');
CodePointer(PyStaticMethod_Type)        := Import('PyStaticMethod_Type');
CodePointer(PySuper_Type)               := Import('PySuper_Type');
CodePointer(PyTraceBack_Type)           := Import('PyTraceBack_Type');
CodePointer(PyWrapperDescr_Type)        := Import('PyWrapperDescr_Type');
CodePointer(_PyWeakref_RefType)         := Import('_PyWeakref_RefType');
CodePointer(_PyWeakref_ProxyType)       := Import('_PyWeakref_ProxyType');
CodePointer(_PyWeakref_CallableProxyType):=Import('_PyWeakref_CallableProxyType');
CodePointer(PyBool_Type)                := Import('PyBool_Type');
CodePointer(PyEnum_Type)                := Import('PyEnum_Type');
CodePointer(PyComplex_FromCComplex)    := Import('PyComplex_FromCComplex');
CodePointer(PyComplex_FromDoubles)     := Import('PyComplex_FromDoubles');
CodePointer(PyComplex_RealAsDouble)    := Import('PyComplex_RealAsDouble');
CodePointer(PyComplex_ImagAsDouble)    := Import('PyComplex_ImagAsDouble');
CodePointer(PyComplex_AsCComplex)      := Import('PyComplex_AsCComplex');
CodePointer(PyCFunction_GetFunction)   := Import('PyCFunction_GetFunction');
CodePointer(PyCFunction_GetSelf)       := Import('PyCFunction_GetSelf');
CodePointer(PyCallable_Check)          := Import('PyCallable_Check');
CodePointer(PyDict_GetItem)            := Import('PyDict_GetItem');
CodePointer(PyDict_SetItem)            := Import('PyDict_SetItem');
CodePointer(PyDict_DelItem)            := Import('PyDict_DelItem');
CodePointer(PyDict_Clear)              := Import('PyDict_Clear');
CodePointer(PyDict_Next)               := Import('PyDict_Next');
CodePointer(PyDict_Keys)               := Import('PyDict_Keys');
CodePointer(PyDict_Values)             := Import('PyDict_Values');
CodePointer(PyDict_Items)              := Import('PyDict_Items');
CodePointer(PyDict_Size)               := Import('PyDict_Size');
CodePointer(PyDict_DelItemString)      := Import('PyDict_DelItemString');
CodePointer(PyDict_Copy)               := Import('PyDict_Copy');
CodePointer(PyDictProxy_New)           := Import('PyDictProxy_New');
CodePointer(PyModule_Create2)          := Import('PyModule_Create2');
CodePointer(PyErr_Print)               := Import('PyErr_Print');
CodePointer(PyErr_SetNone)             := Import('PyErr_SetNone');
CodePointer(PyErr_SetObject)           := Import('PyErr_SetObject');
CodePointer(PyErr_Restore)             := Import('PyErr_Restore');
CodePointer(PyErr_BadArgument)         := Import('PyErr_BadArgument');
CodePointer(PyErr_NoMemory)            := Import('PyErr_NoMemory');
CodePointer(PyErr_SetFromErrno)        := Import('PyErr_SetFromErrno');
CodePointer(PyErr_BadInternalCall)     := Import('PyErr_BadInternalCall');
CodePointer(PyErr_CheckSignals)        := Import('PyErr_CheckSignals');
CodePointer(PyErr_Occurred)            := Import('PyErr_Occurred');
CodePointer(PyErr_Clear)               := Import('PyErr_Clear');
CodePointer(PyErr_Fetch)               := Import('PyErr_Fetch');
CodePointer(PyErr_SetString)           := Import('PyErr_SetString');
CodePointer(PyEval_GetBuiltins)        := Import('PyEval_GetBuiltins');
CodePointer(PyImport_GetModuleDict)    := Import('PyImport_GetModuleDict');
CodePointer(PyArg_Parse)               := Import('PyArg_Parse');
CodePointer(PyArg_ParseTuple)          := Import('PyArg_ParseTuple');
CodePointer(Py_BuildValue)             := Import('Py_BuildValue');
CodePointer(Py_Initialize)             := Import('Py_Initialize');
CodePointer(PyDict_New)                := Import('PyDict_New');
CodePointer(PyDict_SetItemString)      := Import('PyDict_SetItemString');
CodePointer(PyModule_GetDict)          := Import('PyModule_GetDict');
CodePointer(PyObject_Str)              := Import('PyObject_Str');
CodePointer(PyRun_String)              := Import('PyRun_String');
CodePointer(PyRun_SimpleString)        := Import('PyRun_SimpleString');
CodePointer(PyDict_GetItemString)      := Import('PyDict_GetItemString');
CodePointer(PyBytes_AsString)          := Import('PyBytes_AsString');
CodePointer(PyBytes_AsStringAndSize)   := Import('PyBytes_AsStringAndSize');
CodePointer(PySys_SetArgv)             := Import('PySys_SetArgv');
CodePointer(Py_Exit)                   := Import('Py_Exit');
CodePointer(PyCFunction_NewEx)         := Import('PyCFunction_NewEx');
CodePointer(PyEval_CallObjectWithKeywords):=Import('PyEval_CallObjectWithKeywords');
CodePointer(PyEval_GetFrame)           :=Import('PyEval_GetFrame');
CodePointer(PyEval_GetGlobals)         :=Import('PyEval_GetGlobals');
CodePointer(PyEval_GetLocals)          :=Import('PyEval_GetLocals');
CodePointer(PyEval_InitThreads)        :=Import('PyEval_InitThreads');
CodePointer(PyEval_RestoreThread)      :=Import('PyEval_RestoreThread');
CodePointer(PyEval_SaveThread)         :=Import('PyEval_SaveThread');
CodePointer(PyFile_GetLine)            :=Import('PyFile_GetLine');
CodePointer(PyFile_WriteObject)        :=Import('PyFile_WriteObject');
CodePointer(PyFile_WriteString)        :=Import('PyFile_WriteString');
CodePointer(PyFloat_AsDouble)          :=Import('PyFloat_AsDouble');
CodePointer(PyFloat_FromDouble)        :=Import('PyFloat_FromDouble');
CodePointer(PyFunction_GetCode)        :=Import('PyFunction_GetCode');
CodePointer(PyFunction_GetGlobals)     :=Import('PyFunction_GetGlobals');
CodePointer(PyFunction_New)            :=Import('PyFunction_New');
CodePointer(PyImport_AddModule)        :=Import('PyImport_AddModule');
//CodePointer(PyImport_Cleanup)          :=Import('PyImport_Cleanup');
CodePointer(PyImport_GetMagicNumber)   :=Import('PyImport_GetMagicNumber');
CodePointer(PyImport_ImportFrozenModule):=Import('PyImport_ImportFrozenModule');
CodePointer(PyImport_ImportModule)     :=Import('PyImport_ImportModule');
CodePointer(PyImport_Import)           :=Import('PyImport_Import');
CodePointer(PyImport_ReloadModule)     :=Import('PyImport_ReloadModule');
CodePointer(PyLong_AsLong)              :=Import('PyLong_AsLong');
CodePointer(PyList_Append)             :=Import('PyList_Append');
CodePointer(PyList_AsTuple)            :=Import('PyList_AsTuple');
CodePointer(PyList_GetItem)            :=Import('PyList_GetItem');
CodePointer(PyList_GetSlice)           :=Import('PyList_GetSlice');
CodePointer(PyList_Insert)             :=Import('PyList_Insert');
CodePointer(PyList_New)                :=Import('PyList_New');
CodePointer(PyList_Reverse)            :=Import('PyList_Reverse');
CodePointer(PyList_SetItem)            :=Import('PyList_SetItem');
CodePointer(PyList_SetSlice)           :=Import('PyList_SetSlice');
CodePointer(PyList_Size)               :=Import('PyList_Size');
CodePointer(PyList_Sort)               :=Import('PyList_Sort');
CodePointer(PyLong_AsDouble)           :=Import('PyLong_AsDouble');
CodePointer(PyLong_AsLong)             :=Import('PyLong_AsLong');
CodePointer(PyLong_FromDouble)         :=Import('PyLong_FromDouble');
CodePointer(PyLong_FromLong)           :=Import('PyLong_FromLong');
CodePointer(PyLong_FromString)         :=Import('PyLong_FromString');
CodePointer(PyLong_FromString)         :=Import('PyLong_FromString');
CodePointer(PyLong_FromUnsignedLong)   :=Import('PyLong_FromUnsignedLong');
CodePointer(PyLong_AsUnsignedLong)     :=Import('PyLong_AsUnsignedLong');
CodePointer(PyLong_FromUnicode)        :=Import('PyLong_FromUnicode');
CodePointer(PyLong_FromLongLong)       :=Import('PyLong_FromLongLong');
CodePointer(PyLong_AsLongLong)         :=Import('PyLong_AsLongLong');
CodePointer(PyMapping_Check)           :=Import('PyMapping_Check');
CodePointer(PyMapping_GetItemString)   :=Import('PyMapping_GetItemString');
CodePointer(PyMapping_HasKey)          :=Import('PyMapping_HasKey');
CodePointer(PyMapping_HasKeyString)    :=Import('PyMapping_HasKeyString');
CodePointer(PyMapping_Length)          :=Import('PyMapping_Length');
CodePointer(PyMapping_SetItemString)   :=Import('PyMapping_SetItemString');
CodePointer(PyMethod_Function)         :=Import('PyMethod_Function');
CodePointer(PyMethod_New)              :=Import('PyMethod_New');
CodePointer(PyMethod_Self)             :=Import('PyMethod_Self');
CodePointer(PyModule_GetName)          :=Import('PyModule_GetName');
CodePointer(PyModule_New)              :=Import('PyModule_New');
CodePointer(PyNumber_Absolute)         :=Import('PyNumber_Absolute');
CodePointer(PyNumber_Add)              :=Import('PyNumber_Add');
CodePointer(PyNumber_And)              :=Import('PyNumber_And');
CodePointer(PyNumber_Check)            :=Import('PyNumber_Check');
CodePointer(PyNumber_TrueDivide)           :=Import('PyNumber_TrueDivide');
CodePointer(PyNumber_FloorDivide)      :=Import('PyNumber_FloorDivide');
CodePointer(PyNumber_TrueDivide)       :=Import('PyNumber_TrueDivide');
CodePointer(PyNumber_Divmod)           :=Import('PyNumber_Divmod');
CodePointer(PyNumber_Float)            :=Import('PyNumber_Float');
CodePointer(PyNumber_Invert)           :=Import('PyNumber_Invert');
CodePointer(PyNumber_Long)             :=Import('PyNumber_Long');
CodePointer(PyNumber_Lshift)           :=Import('PyNumber_Lshift');
CodePointer(PyNumber_Multiply)         :=Import('PyNumber_Multiply');
CodePointer(PyNumber_Negative)         :=Import('PyNumber_Negative');
CodePointer(PyNumber_Or)               :=Import('PyNumber_Or');
CodePointer(PyNumber_Positive)         :=Import('PyNumber_Positive');
CodePointer(PyNumber_Power)            :=Import('PyNumber_Power');
CodePointer(PyNumber_Remainder)        :=Import('PyNumber_Remainder');
CodePointer(PyNumber_Rshift)           :=Import('PyNumber_Rshift');
CodePointer(PyNumber_Subtract)         :=Import('PyNumber_Subtract');
CodePointer(PyNumber_Xor)              :=Import('PyNumber_Xor');
CodePointer(PyOS_InitInterrupts)       :=Import('PyOS_InitInterrupts');
CodePointer(PyOS_InterruptOccurred)    :=Import('PyOS_InterruptOccurred');
CodePointer(PyObject_CallObject)       :=Import('PyObject_CallObject');
CodePointer(PyObject_CallMethod)       :=Import('PyObject_CallMethod');
CodePointer(PyObject_RichCompare)      :=Import('PyObject_RichCompare');
CodePointer(PyObject_RichCompareBool)  :=Import('PyObject_RichCompareBool');
CodePointer(PyObject_GetAttr)          :=Import('PyObject_GetAttr');
CodePointer(PyObject_GetAttrString)    :=Import('PyObject_GetAttrString');
CodePointer(PyObject_GetItem)          :=Import('PyObject_GetItem');
CodePointer(PyObject_DelItem)          :=Import('PyObject_DelItem');
CodePointer(PyObject_HasAttrString)    :=Import('PyObject_HasAttrString');
CodePointer(PyObject_Hash)             :=Import('PyObject_Hash');
CodePointer(PyObject_IsTrue)           :=Import('PyObject_IsTrue');
CodePointer(PyObject_Length)           :=Import('PyObject_Length');
CodePointer(PyObject_Repr)             :=Import('PyObject_Repr');
CodePointer(PyObject_SetAttr)          :=Import('PyObject_SetAttr');
CodePointer(PyObject_SetAttrString)    :=Import('PyObject_SetAttrString');
CodePointer(PyObject_SetItem)          :=Import('PyObject_SetItem');
CodePointer(PyObject_Init)             :=Import('PyObject_Init');
CodePointer(PyObject_InitVar)          :=Import('PyObject_InitVar');
CodePointer(PyObject_New)              :=Import('_PyObject_New');
CodePointer(PyObject_NewVar)           :=Import('_PyObject_NewVar');
CodePointer(PyObject_Free)             :=Import('PyObject_Free');
CodePointer(PyObject_GetIter)          :=Import('PyObject_GetIter');
CodePointer(PyIter_Next)               :=Import('PyIter_Next');
CodePointer(PyObject_IsInstance)       :=Import('PyObject_IsInstance');
CodePointer(PyObject_IsSubclass)       :=Import('PyObject_IsSubclass');
CodePointer(PyObject_Call)             :=Import('PyObject_Call');
CodePointer(PyObject_GenericGetAttr)   :=Import('PyObject_GenericGetAttr');
CodePointer(PyObject_GenericSetAttr)   :=Import('PyObject_GenericSetAttr');
CodePointer(PyObject_GC_Malloc)         :=Import('_PyObject_GC_Malloc');
CodePointer(PyObject_GC_New)            :=Import('_PyObject_GC_New');
CodePointer(PyObject_GC_NewVar)         :=Import('_PyObject_GC_NewVar');
CodePointer(PyObject_GC_Resize)         :=Import('_PyObject_GC_Resize');
CodePointer(PyObject_GC_Del)            :=Import('PyObject_GC_Del');
CodePointer(PyObject_GC_Track)          :=Import('PyObject_GC_Track');
CodePointer(PyObject_GC_UnTrack)        :=Import('PyObject_GC_UnTrack');
CodePointer(PySequence_Check)          :=Import('PySequence_Check');
CodePointer(PySequence_Concat)         :=Import('PySequence_Concat');
CodePointer(PySequence_Count)          :=Import('PySequence_Count');
CodePointer(PySequence_GetItem)        :=Import('PySequence_GetItem');
CodePointer(PySequence_GetSlice)       :=Import('PySequence_GetSlice');
CodePointer(PySequence_In)             :=Import('PySequence_In');
CodePointer(PySequence_Index)          :=Import('PySequence_Index');
CodePointer(PySequence_Length)         :=Import('PySequence_Length');
CodePointer(PySequence_Repeat)         :=Import('PySequence_Repeat');
CodePointer(PySequence_SetItem)        :=Import('PySequence_SetItem');
CodePointer(PySequence_SetSlice)       :=Import('PySequence_SetSlice');
CodePointer(PySequence_DelSlice)       :=Import('PySequence_DelSlice');
CodePointer(PySequence_Tuple)          :=Import('PySequence_Tuple');
CodePointer(PySequence_Contains)       :=Import('PySequence_Contains');
CodePointer(PySlice_GetIndices)        :=Import('PySlice_GetIndices');
CodePointer(PySeqIter_New)             :=Import('PySeqIter_New');
CodePointer(PySlice_GetIndicesEx)      :=Import('PySlice_GetIndicesEx');
CodePointer(PySlice_New)               :=Import('PySlice_New');
CodePointer(PyBytes_Concat)           :=Import('PyBytes_Concat');
CodePointer(PyBytes_ConcatAndDel)     :=Import('PyBytes_ConcatAndDel');
CodePointer(PyBytes_FromStringAndSize):=Import('PyBytes_FromStringAndSize');
CodePointer(PyBytes_Size)             :=Import('PyBytes_Size');
CodePointer(PyBytes_DecodeEscape)     :=Import('PyBytes_DecodeEscape');
CodePointer(PyBytes_Repr)             :=Import('PyBytes_Repr');
CodePointer(PySys_GetObject)           :=Import('PySys_GetObject');
CodePointer(PySys_SetObject)           :=Import('PySys_SetObject');
CodePointer(PySys_SetPath)             :=Import('PySys_SetPath');
CodePointer(PyTraceBack_Here)          :=Import('PyTraceBack_Here');
CodePointer(PyTraceBack_Print)         :=Import('PyTraceBack_Print');
CodePointer(PyTuple_GetItem)           :=Import('PyTuple_GetItem');
CodePointer(PyTuple_GetSlice)          :=Import('PyTuple_GetSlice');
CodePointer(PyTuple_New)               :=Import('PyTuple_New');
CodePointer(PyTuple_SetItem)           :=Import('PyTuple_SetItem');
CodePointer(PyTuple_Size)              :=Import('PyTuple_Size');
CodePointer(PyType_IsSubtype)          :=Import('PyType_IsSubtype');
CodePointer(PyType_GenericAlloc)       :=Import('PyType_GenericAlloc');
CodePointer(PyType_GenericNew)         :=Import('PyType_GenericNew');
CodePointer(PyType_Ready)              :=Import('PyType_Ready');
CodePointer(PyUnicode_FromWideChar)    :=Import('PyUnicode_FromWideChar');
CodePointer(PyUnicode_AsWideChar)      :=Import('PyUnicode_AsWideChar');
CodePointer(PyUnicode_Decode)          :=Import('PyUnicode_Decode');
CodePointer(PyUnicode_AsEncodedString) :=Import('PyUnicode_AsEncodedString');
CodePointer(PyUnicode_FromOrdinal)     :=Import('PyUnicode_FromOrdinal');
CodePointer(PyUnicode_GetSize)         :=Import('PyUnicode_GetSize');
CodePointer(PyWeakref_GetObject)       :=Import('PyWeakref_GetObject');
CodePointer(PyWeakref_NewProxy)        :=Import('PyWeakref_NewProxy');
CodePointer(PyWeakref_NewRef)          :=Import('PyWeakref_NewRef');
CodePointer(PyWrapper_New)             :=Import('PyWrapper_New');
CodePointer(PyBool_FromLong)           :=Import('PyBool_FromLong');
CodePointer(PyThreadState_SetAsyncExc) :=Import('PyThreadState_SetAsyncExc');
CodePointer(Py_AtExit)                 :=Import('Py_AtExit');
CodePointer(Py_FatalError)             :=Import('Py_FatalError');
//CodePointer(_PyBytes_Resize)           :=Import('_PyBytes_Resize');
CodePointer(Py_CompileStringExFlags)   :=Import('Py_CompileStringExFlags');
CodePointer(_PyObject_New)             :=Import('_PyObject_New');
CodePointer(Py_Finalize)                :=Import('Py_Finalize');
CodePointer(PyErr_ExceptionMatches)     :=Import('PyErr_ExceptionMatches');
CodePointer(PyErr_GivenExceptionMatches):=Import('PyErr_GivenExceptionMatches');
CodePointer(PyEval_EvalCode)            :=Import('PyEval_EvalCode');
CodePointer(Py_GetVersion)              :=Import('Py_GetVersion');
CodePointer(Py_GetCopyright)            :=Import('Py_GetCopyright');
CodePointer(Py_GetExecPrefix)           :=Import('Py_GetExecPrefix');
CodePointer(Py_GetPath)                 :=Import('Py_GetPath');
CodePointer(Py_SetPythonHome)           :=Import('Py_SetPythonHome');
CodePointer(Py_GetPythonHome)           :=Import('Py_GetPythonHome');
CodePointer(Py_DecodeLocale)            :=Import('Py_DecodeLocale');
CodePointer(Py_GetPrefix)               :=Import('Py_GetPrefix');
CodePointer(Py_GetProgramName)          :=Import('Py_GetProgramName');
CodePointer(PyParser_SimpleParseStringFlags) := Import('PyParser_SimpleParseStringFlags');
CodePointer(PyNode_Free)                :=Import('PyNode_Free');
CodePointer(PyErr_NewException)         :=Import('PyErr_NewException');
CodePointer(PyMem_Malloc)              := Import ('PyMem_Malloc');
CodePointer(Py_SetProgramName)        := Import('Py_SetProgramName');
CodePointer(Py_IsInitialized)         := Import('Py_IsInitialized');
CodePointer(Py_GetProgramFullPath)    := Import('Py_GetProgramFullPath');
CodePointer(Py_NewInterpreter)        := Import('Py_NewInterpreter');
CodePointer(Py_EndInterpreter)        := Import('Py_EndInterpreter');
CodePointer(PyEval_AcquireLock)       := Import('PyEval_AcquireLock');
CodePointer(PyEval_ReleaseLock)       := Import('PyEval_ReleaseLock');
CodePointer(PyEval_AcquireThread)     := Import('PyEval_AcquireThread');
CodePointer(PyEval_ReleaseThread)     := Import('PyEval_ReleaseThread');
CodePointer(PyInterpreterState_New)   := Import('PyInterpreterState_New');
CodePointer(PyInterpreterState_Clear) := Import('PyInterpreterState_Clear');
CodePointer(PyInterpreterState_Delete):= Import('PyInterpreterState_Delete');
CodePointer(PyThreadState_New)        := Import('PyThreadState_New');
CodePointer(PyThreadState_Clear)      := Import('PyThreadState_Clear');
CodePointer(PyThreadState_Delete)     := Import('PyThreadState_Delete');
CodePointer(PyThreadState_Get)        := Import('PyThreadState_Get');
CodePointer(PyThreadState_Swap)       := Import('PyThreadState_Swap');
CodePointer(PyErr_SetInterrupt)       := Import('PyErr_SetInterrupt');
CodePointer(PyGILState_Ensure)        := Import('PyGILState_Ensure');
CodePointer(PyGILState_Release)       := Import('PyGILState_Release');
