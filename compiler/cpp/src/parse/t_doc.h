#ifndef T_DOC_H
#define T_DOC_H

/**
 *
 *
 * @author Charlie Cheever <charlie@facebook.com>
 */
class t_doc {

  public:

    void set_doc(const std::string& doc) {
      doc_ = doc;
      has_doc_ = true;
    }

    const std::string& get_doc() const {
      return doc_;
    }

    bool has_doc() {
      return has_doc_;
    }

  private:
    std::string doc_;
    bool has_doc_;

};

#endif
