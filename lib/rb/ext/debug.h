

#ifndef DEBUG
  #error Missing debug level, #define DEBUG
#endif

#if DEBUG
  #define DEPTH_INCREMENT 2

  extern char separator[];
  extern int curdepth;

  #define DEBUGS(s) printf("%s%s\n", separator, s)
  #define DEBUGF(fmt, ...) printf("%s", separator), printf(fmt, __VA_ARGS__), printf("\n")

  #define DEBUG_FUNCTION_ENTRY() DEBUGF("->%s:%s", __FILE__, __FUNCTION__), separator[curdepth] = ' ', curdepth+=DEPTH_INCREMENT, separator[curdepth] = 0
  #define DEBUG_FUNCTION_EXIT()  separator[curdepth] = ' ', curdepth-=DEPTH_INCREMENT, separator[curdepth] = 0, DEBUGF("<-%s:%s", __FILE__, __FUNCTION__)
  #define DEBUG_FUNCTION_PROGRESS() DEBUGF("%s, %s:%d", __FILE__, __FUNCTION__, __LINE__)
#else
  #define DEBUGS(s)
  #define DEBUGF(fmt, ...)

  #define DEBUG_FUNCTION_ENTRY()
  #define DEBUG_FUNCTION_PROGRESS() 
  #define DEBUG_FUNCTION_EXIT()

#endif