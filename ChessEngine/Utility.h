// noexcept macro for compilers without noexcept *cough cough VC12 cough*
#if NOEXCEPT_SUPPORTED
#define NOEXCEPT noexcept
#else
#define NOEXCEPT throw()
#endif