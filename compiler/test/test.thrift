typedef i32 gozone_t

enum numberz
{
  ONE = 1,
  TWO,
  THREE,
  FIVE = 5,
  SIX,
  EIGHT = 8
}

struct things
{
  u32 first_num,
  u32 second_num,
  u32 third_num,
  u32 fourth_num
}

service worker
{
  void   funky_void()
  string funky_string(string thing)
  u32    funky_u32(u32 thing)
  i32    funky_i32(i32 thing)
}
