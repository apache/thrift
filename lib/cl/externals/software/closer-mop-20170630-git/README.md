# Closer to MOP
Closer to MOP is a compatibility layer that rectifies many of the absent or incorrect CLOS MOP features across a broad range of Common Lisp implementations.

Closer to MOP is also provided by [Quicklisp](https://www.quicklisp.org/).

Currently, the following Common Lisp implementations are supported:
* Allegro Common Lisp 10.1 Express Edition
* Armed Bear Common Lisp 1.5.0
* CLisp 2.49
* Clozure Common Lisp 1.11
* CMU Common Lisp 21b
* Embeddable Common Lisp 16.1.3
* LispWorks 6.1.1 Personal Edition
* LispWorks 7.0.0 Hobbyist Edition
* Steel Bank Common Lisp 1.3.18

The following implementations were supported in the past:
* Allegro Common Lisp 7.0, 8.0 - 8.2 Enterprise Editions
* Allegro Common Lisp 9.0, 10.0 Express Editions
* Armed Bear Common Lisp 1.1.1, 1.2.1, 1.3.0-1.3.3, 1.4.0
* CLisp from 2.35 onward
* Clozure Common Lisp 1.2 - 1.10
* CMU Common Lisp 19c-f, 20a-f, 21a
* Embeddable Common Lisp 9.12.3, 10.3.1, 10.4.1, 11.1.1, 12.2.1, 12.12.1, 15.2.21, 15.3.7, 16.0.0, 16.1.2
* LispWorks 4.3 & 4.4, Personal and Professional Editions
* LispWorks 5.0.1, 5.0.2, 5.1.0 - 5.1.2 Personal and Professional Editions
* LispWorks 6.0, 6.0.1 Enterprise Editions
* LispWorks 6.1, 6.1.1, Professional Editions
* Macintosh Common Lisp 5.1, 5.2.1
* OpenMCL 1.0
* Scieneer Common Lisp 1.3.9
* Steel Bank Common Lisp from 0.9.16 onward (except version 1.0.0)

The respective code conditionalizations are still in the source files, so there is a good chance that they still work, especially for current or newer versions. However, there is no guarantee that this is the case, and active work for these implementations is currently on hold.

New in version 1.0.0:
* New version number based on semantic versioning.
* Since version 0.61, support for Allegro Common Lisp 8.2 & 9.0, ABCL, and LispWorks 6.1 has been added.
* ECL 12.12.1 has seen major improvements in its MOP support, and therefore also in Closer to MOP.
* Several bug fixes.

New in version 0.61:
* Added support for LispWorks 6.0.

Highlights of version 0.6:
* Completely reworked support for Embeddable Common Lisp.
* Resurrected support for Macintosh Common Lisp (now RMCL).
* Added partial support for Scieneer Common Lisp.
* Closer to MOP now recognizes and supports 9 different Common Lisp implementations!
* Added improved and complete generic function invocation protocols to Clozure Common Lisp, CLisp, ECL, LispWorks and SBCL. This includes support for COMPUTE-EFFECTIVE-METHOD-FUNCTION (a piece missing in AMOP) and MAKE-METHOD-LAMBDA in all these implementations (except for ECL, where I currently cannot support MAKE-METHOD-LAMBDA). Note: in order to ensure that MAKE-METHOD-LAMBDA doesn't create surprising results (or better: surprisingly doesn't create the results you expect), it is now ensured that DEFGENERIC creates a generic function metaobject in the compilation environment (without the method definitions), and it is now checked in DEFMETHOD that such a generic function metaobject exists for the method to be defined. If such a generic function metaobject doesn't exist, a STYLE-WARNING is issued (except for SBCL, which itself already issues a STYLE-WARNING in this case).
* The standard metobject definition macros and functions (DEFCLASS, DEFGENERIC, DEFMETHOD, ENSURE-CLASS, ENSURE-GENERIC-FUNCTION, etc.) sometimes forced the use of the replacement 'standard' metaobject classes of Closer to MOP (STANDARD-CLASS, STANDARD-GENERIC-FUNCTION and STANDARD-METHOD). This is now completely removed: If you don't use a :METACLASS or :GENERIC-FUNCTION-CLASS option explicitly, these defining operators will use the internal metaclasses of the respective Common Lisp implementation, under the assumption that they are usually more efficient than the replacements in Closer to MOP. If for some reason, you want to ensure to use the replacements, you have to do so explicitly. (Note: The main purpose of the replacements is to provide a common compatible basis for your own metaobject subclasses, not to be used in their own right.)
* Replaced synchronization statements in Allegro and LispWorks with versions that will be compatible with their future SMP support.
* In LispWorks, automatically generated slot readers and writers now only call SLOT-VALUE-USING-CLASS and (SETF SLOT-VALUE-USING-CLASS) if there are actually definitions available for them. Otherwise, they use the native optimized slot access.
* Simplified and improved conditionalizations for Clozure Common Lisp and LispWorks, and removed mentions of OpenMCL (which was just the old name for Clozure Common Lisp).
* Reorganized the code: Moved all package definitions into one place, moved shared code into one common file, and removed the subfolders per CL implementations. (This is mostly to make the maintainer's job easier.)
* Added an Allegro-specific system definition.
* Lots of small little bug fixes and improvements here and there.
* Extra special thanks to Duane Rettig, Steve Haflich, and Juan Jose Garcia-Ripoll for fixing extra hard bugs in extra short amount of time.

Highlights of version 0.55:
* Added standard-instance-access and funcallable-standard-instance-access to LispWorks, due to popular request.
* Added a utility function subclassp that is sometimes more robust than subtypep (but subtypep should be preferred whenever possible).

Highlights of version 0.5:
* Ensured that a defgeneric form makes a generic function metaobject available in the compile-time environment. Otherwise, defmethod may not yield a method of the correct method metaobject class.
* Removed dependency on lw-compat.
* Added support for compute-discriminating-function in Clozure Common Lisp and OpenMCL, based on code provided by Slava Akhmechet.
* Added a classp predicate, due to Willem Broekema.

Highlights of version 0.4:
* Utility function REQUIRED-ARGS added for collecting the required arguments of a lambda list.
* Utility function ENSURE-FINALIZED added for ensuring that a class metaobject is finalized.

Closer to MOP has an asdf system definition, and is part of Quicklisp, so it should be straightforward to include it in your own projects. The package that exports the relevant symbols is called CLOSER-MOP or short C2MOP.

Note that in some cases, symbols from the underlying MOP implementation or even the underlying COMMON-LISP package are shadowed in Closer to MOP. So if you use the CLOSER-MOP package you may need to shadow-import those symbols. Alternatively, you can use the packages CLOSER-COMMON-LISP and CLOSER-COMMON-LISP-USER that provide the symbols of COMMON-LISP / COMMON-LISP-USER plus the symbols of the CLOS MOP and the necessary shadow-imported symbols.

If symbols from the underlying MOP implementation or the COMMON-LISP package are shadowed in Closer to MOP, if they are names for metaobject classes, they are supposed to be used primarily for subclassing. If in rare cases you want to refer to them directly, please be advised that you may need to make fine-grained distinctions between the original symbols and the shadowed symbols, depending on context. The restrictions in the CLOS MOP specification do not allow for a more convenient solution.

For details on what has or has not been fixed, see the accompanying file features.txt. Please also check the comments that come with the source code.
