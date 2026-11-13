/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#ifndef THRIFT_TEST_CPP_TESTOPTIONS_H
#define THRIFT_TEST_CPP_TESTOPTIONS_H

#include <cstddef>
#include <functional>
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

// Command line options for the cross-test client and server: --name value,
// --name=value, -x value and -xvalue, and unique prefixes of long names, as
// boost::program_options accepts them by default.
class TestOptions {
public:
  explicit TestOptions(const std::string& caption) : caption_(caption) {}

  // An option without a value, such as "help,h": a long name and an
  // optional one-letter short name.
  TestOptions& flag(const char* names, const char* help) {
    add(names, help, nullptr, std::string());
    return *this;
  }

  // An option that stores its value in *target. The help text shows the
  // current value of *target as the default unless showDefault is false.
  template <typename T>
  TestOptions& value(const char* names, T* target, const char* help = "", bool showDefault = true) {
    std::ostringstream shown;
    if (showDefault) {
      shown << *target;
    }
    add(
        names, help,
        [target](const std::string& name, const std::string& text) { convert(name, text, target); },
        showDefault ? "(=" + shown.str() + ")" : std::string());
    return *this;
  }

  // Throws std::invalid_argument for an unknown option, a missing or
  // malformed value, a value given to a flag or a positional argument.
  void parse(int argc, char** argv) {
    for (int i = 1; i < argc; ++i) {
      const std::string arg = argv[i];
      Option* option = nullptr;
      std::string value;
      bool hasValue = false;
      if (arg.compare(0, 2, "--") == 0 && arg.size() > 2) {
        std::string name = arg.substr(2);
        const std::string::size_type eq = name.find('=');
        if (eq != std::string::npos) {
          value = name.substr(eq + 1);
          hasValue = true;
          name.erase(eq);
        }
        option = findLong(name);
      } else if (arg[0] == '-' && arg.size() > 1) {
        option = findShort(arg[1]);
        if (arg.size() > 2) {
          value = arg.substr(2);
          hasValue = true;
        }
      } else {
        throw std::invalid_argument("unexpected argument '" + arg + "'");
      }
      if (!option->store) {
        if (hasValue) {
          throw std::invalid_argument("option '--" + option->name + "' takes no value");
        }
      } else {
        if (!hasValue) {
          if (i + 1 >= argc || isOption(argv[i + 1])) {
            throw std::invalid_argument("option '--" + option->name + "' needs a value");
          }
          value = argv[++i];
        }
        option->store(option->name, value);
      }
      given_.push_back(option->name);
    }
  }

  // Whether the option with this long name was given on the command line.
  bool count(const std::string& name) const {
    for (const std::string& given : given_) {
      if (given == name) {
        return true;
      }
    }
    return false;
  }

  friend std::ostream& operator<<(std::ostream& out, const TestOptions& options) {
    std::vector<std::string> left;
    std::size_t width = 0;
    for (const Option& option : options.options_) {
      std::string text = option.shortName
                             ? std::string("-") + option.shortName + " [ --" + option.name + " ]"
                             : "--" + option.name;
      if (option.store) {
        text += " arg";
        if (!option.shownDefault.empty()) {
          text += " " + option.shownDefault;
        }
      }
      width = text.size() > width ? text.size() : width;
      left.push_back(text);
    }
    out << options.caption_ << ":\n";
    for (std::size_t i = 0; i < left.size(); ++i) {
      out << "  " << left[i];
      if (!options.options_[i].help.empty()) {
        out << std::string(width - left[i].size() + 2, ' ') << options.options_[i].help;
      }
      out << '\n';
    }
    return out;
  }

private:
  typedef std::function<void(const std::string&, const std::string&)> Store;

  struct Option {
    std::string name;
    char shortName;
    std::string help;
    Store store;
    std::string shownDefault;
  };

  void add(const char* names, const char* help, Store store, const std::string& shownDefault) {
    Option option;
    option.name = names;
    option.shortName = '\0';
    const std::string::size_type comma = option.name.find(',');
    if (comma != std::string::npos) {
      option.shortName = option.name[comma + 1];
      option.name.erase(comma);
    }
    option.help = help;
    option.store = store;
    option.shownDefault = shownDefault;
    options_.push_back(option);
  }

  // A following "--name" or "-x" is an option, not a value; "-1" is a value.
  static bool isOption(const char* arg) {
    return arg[0] == '-' && arg[1] != '\0' && (arg[1] < '0' || arg[1] > '9');
  }

  Option* findLong(const std::string& name) {
    Option* match = nullptr;
    bool ambiguous = false;
    for (Option& option : options_) {
      if (option.name == name) {
        return &option;
      }
      if (option.name.compare(0, name.size(), name) == 0) {
        ambiguous = match != nullptr;
        match = &option;
      }
    }
    if (ambiguous) {
      throw std::invalid_argument("option '--" + name + "' is ambiguous");
    }
    if (!match) {
      throw std::invalid_argument("unrecognised option '--" + name + "'");
    }
    return match;
  }

  Option* findShort(char shortName) {
    for (Option& option : options_) {
      if (option.shortName == shortName) {
        return &option;
      }
    }
    throw std::invalid_argument(std::string("unrecognised option '-") + shortName + "'");
  }

  static void convert(const std::string& name, const std::string& text, std::string* target) {
    (void)name;
    *target = text;
  }

  template <typename T>
  static void convert(const std::string& name, const std::string& text, T* target) {
    std::istringstream in(text);
    T parsed;
    if (text.empty() || (std::is_unsigned<T>::value && text[0] == '-') || !(in >> parsed)
        || in.peek() != std::char_traits<char>::eof()) {
      throw std::invalid_argument("the argument ('" + text + "') for option '--" + name
                                  + "' is invalid");
    }
    *target = parsed;
  }

  std::string caption_;
  std::vector<Option> options_;
  std::vector<std::string> given_;
};

#endif // THRIFT_TEST_CPP_TESTOPTIONS_H
