#ifndef T_MAP_H
#define T_MAP_H

class t_map : public t_type {
 public:
  t_map(t_type* key_type, t_type* val_type) :
    key_type_(key_type), val_type_(val_type) {}
  ~t_map() {}

  t_type* get_key_type() const { return key_type_; }
  t_type* get_val_type() const { return val_type_; }
  bool is_map() const { return true; }

 private:
  t_type* key_type_;
  t_type* val_type_;
};

#endif
