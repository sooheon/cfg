{
  "global": {
    "check_for_updates_on_startup": true,
    "show_in_menu_bar": false,
    "show_profile_name_in_menu_bar": false
  },
  "profiles": [
    {
      "complex_modifications": {
        "parameters": {
          "basic.simultaneous_threshold_milliseconds": 50,
          "basic.to_delayed_action_delay_milliseconds": 500,
          "basic.to_if_alone_timeout_milliseconds": 500,
          "basic.to_if_held_down_threshold_milliseconds": 500
        },
        "rules": [
          {
            "description": "SpaceFN Layer",
            "manipulators": [
              {
                "from": {
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  },
                  "simultaneous": [
                    {
                      "key_code": "spacebar"
                    },
                    {
                      "key_code": "b"
                    }
                  ],
                  "simultaneous_options": {
                    "key_down_order": "strict",
                    "key_up_order": "strict_inverse",
                    "to_after_key_up": [
                      {
                        "set_variable": {
                          "name": "SpaceFN",
                          "value": 0
                        }
                      }
                    ]
                  }
                },
                "parameters": {
                  "basic.simultaneous_threshold_milliseconds": 500
                },
                "to": [
                  {
                    "set_variable": {
                      "name": "SpaceFN",
                      "value": 1
                    }
                  },
                  {
                    "key_code": "spacebar"
                  }
                ],
                "type": "basic"
              },
              {
                "conditions": [
                  {
                    "name": "SpaceFN",
                    "type": "variable_if",
                    "value": 1
                  }
                ],
                "from": {
                  "key_code": "b",
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  }
                },
                "to": [
                  {
                    "key_code": "spacebar"
                  }
                ],
                "type": "basic"
              },
              {
                "from": {
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  },
                  "simultaneous": [
                    {
                      "key_code": "spacebar"
                    },
                    {
                      "key_code": "c"
                    }
                  ],
                  "simultaneous_options": {
                    "key_down_order": "strict",
                    "key_up_order": "strict_inverse",
                    "to_after_key_up": [
                      {
                        "set_variable": {
                          "name": "SpaceFN",
                          "value": 0
                        }
                      }
                    ]
                  }
                },
                "parameters": {
                  "basic.simultaneous_threshold_milliseconds": 500
                },
                "to": [
                  {
                    "set_variable": {
                      "name": "SpaceFN",
                      "value": 1
                    }
                  },
                  {
                    "key_code": "escape"
                  }
                ],
                "type": "basic"
              },
              {
                "conditions": [
                  {
                    "name": "SpaceFN",
                    "type": "variable_if",
                    "value": 1
                  }
                ],
                "from": {
                  "key_code": "c",
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  }
                },
                "to": [
                  {
                    "key_code": "escape"
                  }
                ],
                "type": "basic"
              },
              {
                "from": {
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  },
                  "simultaneous": [
                    {
                      "key_code": "spacebar"
                    },
                    {
                      "key_code": "d"
                    }
                  ],
                  "simultaneous_options": {
                    "key_down_order": "strict",
                    "key_up_order": "strict_inverse",
                    "to_after_key_up": [
                      {
                        "set_variable": {
                          "name": "SpaceFN",
                          "value": 0
                        }
                      }
                    ]
                  }
                },
                "parameters": {
                  "basic.simultaneous_threshold_milliseconds": 500
                },
                "to": [
                  {
                    "set_variable": {
                      "name": "SpaceFN",
                      "value": 1
                    }
                  },
                  {
                    "key_code": "page_down"
                  }
                ],
                "type": "basic"
              },
              {
                "conditions": [
                  {
                    "name": "SpaceFN",
                    "type": "variable_if",
                    "value": 1
                  }
                ],
                "from": {
                  "key_code": "d",
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  }
                },
                "to": [
                  {
                    "key_code": "page_down"
                  }
                ],
                "type": "basic"
              },
              {
                "from": {
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  },
                  "simultaneous": [
                    {
                      "key_code": "spacebar"
                    },
                    {
                      "key_code": "u"
                    }
                  ],
                  "simultaneous_options": {
                    "key_down_order": "strict",
                    "key_up_order": "strict_inverse",
                    "to_after_key_up": [
                      {
                        "set_variable": {
                          "name": "SpaceFN",
                          "value": 0
                        }
                      }
                    ]
                  }
                },
                "parameters": {
                  "basic.simultaneous_threshold_milliseconds": 500
                },
                "to": [
                  {
                    "set_variable": {
                      "name": "SpaceFN",
                      "value": 1
                    }
                  },
                  {
                    "key_code": "page_up"
                  }
                ],
                "type": "basic"
              },
              {
                "conditions": [
                  {
                    "name": "SpaceFN",
                    "type": "variable_if",
                    "value": 1
                  }
                ],
                "from": {
                  "key_code": "u",
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  }
                },
                "to": [
                  {
                    "key_code": "page_up"
                  }
                ],
                "type": "basic"
              },
              {
                "from": {
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  },
                  "simultaneous": [
                    {
                      "key_code": "spacebar"
                    },
                    {
                      "key_code": "m"
                    }
                  ],
                  "simultaneous_options": {
                    "key_down_order": "strict",
                    "key_up_order": "strict_inverse",
                    "to_after_key_up": [
                      {
                        "set_variable": {
                          "name": "SpaceFN",
                          "value": 0
                        }
                      }
                    ]
                  }
                },
                "parameters": {
                  "basic.simultaneous_threshold_milliseconds": 500
                },
                "to": [
                  {
                    "set_variable": {
                      "name": "SpaceFN",
                      "value": 1
                    }
                  },
                  {
                    "key_code": "enter"
                  }
                ],
                "type": "basic"
              },
              {
                "conditions": [
                  {
                    "name": "SpaceFN",
                    "type": "variable_if",
                    "value": 1
                  }
                ],
                "from": {
                  "key_code": "m",
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  }
                },
                "to": [
                  {
                    "key_code": "enter"
                  }
                ],
                "type": "basic"
              },
              {
                "from": {
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  },
                  "simultaneous": [
                    {
                      "key_code": "spacebar"
                    },
                    {
                      "key_code": "n"
                    }
                  ],
                  "simultaneous_options": {
                    "key_down_order": "strict",
                    "key_up_order": "strict_inverse",
                    "to_after_key_up": [
                      {
                        "set_variable": {
                          "name": "SpaceFN",
                          "value": 0
                        }
                      }
                    ]
                  }
                },
                "parameters": {
                  "basic.simultaneous_threshold_milliseconds": 500
                },
                "to": [
                  {
                    "set_variable": {
                      "name": "SpaceFN",
                      "value": 1
                    }
                  },
                  {
                    "key_code": "down_arrow"
                  }
                ],
                "type": "basic"
              },
              {
                "conditions": [
                  {
                    "name": "SpaceFN",
                    "type": "variable_if",
                    "value": 1
                  }
                ],
                "from": {
                  "key_code": "n",
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  }
                },
                "to": [
                  {
                    "key_code": "down_arrow"
                  }
                ],
                "type": "basic"
              },
              {
                "from": {
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  },
                  "simultaneous": [
                    {
                      "key_code": "spacebar"
                    },
                    {
                      "key_code": "p"
                    }
                  ],
                  "simultaneous_options": {
                    "key_down_order": "strict",
                    "key_up_order": "strict_inverse",
                    "to_after_key_up": [
                      {
                        "set_variable": {
                          "name": "SpaceFN",
                          "value": 0
                        }
                      }
                    ]
                  }
                },
                "parameters": {
                  "basic.simultaneous_threshold_milliseconds": 500
                },
                "to": [
                  {
                    "set_variable": {
                      "name": "SpaceFN",
                      "value": 1
                    }
                  },
                  {
                    "key_code": "up_arrow"
                  }
                ],
                "type": "basic"
              },
              {
                "conditions": [
                  {
                    "name": "SpaceFN",
                    "type": "variable_if",
                    "value": 1
                  }
                ],
                "from": {
                  "key_code": "p",
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  }
                },
                "to": [
                  {
                    "key_code": "up_arrow"
                  }
                ],
                "type": "basic"
              },
              {
                "from": {
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  },
                  "simultaneous": [
                    {
                      "key_code": "spacebar"
                    },
                    {
                      "key_code": "j"
                    }
                  ],
                  "simultaneous_options": {
                    "key_down_order": "strict",
                    "key_up_order": "strict_inverse",
                    "to_after_key_up": [
                      {
                        "set_variable": {
                          "name": "SpaceFN",
                          "value": 0
                        }
                      }
                    ]
                  }
                },
                "parameters": {
                  "basic.simultaneous_threshold_milliseconds": 500
                },
                "to": [
                  {
                    "set_variable": {
                      "name": "SpaceFN",
                      "value": 1
                    }
                  },
                  {
                    "key_code": "down_arrow"
                  }
                ],
                "type": "basic"
              },
              {
                "conditions": [
                  {
                    "name": "SpaceFN",
                    "type": "variable_if",
                    "value": 1
                  }
                ],
                "from": {
                  "key_code": "j",
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  }
                },
                "to": [
                  {
                    "key_code": "down_arrow"
                  }
                ],
                "type": "basic"
              },
              {
                "from": {
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  },
                  "simultaneous": [
                    {
                      "key_code": "spacebar"
                    },
                    {
                      "key_code": "k"
                    }
                  ],
                  "simultaneous_options": {
                    "key_down_order": "strict",
                    "key_up_order": "strict_inverse",
                    "to_after_key_up": [
                      {
                        "set_variable": {
                          "name": "SpaceFN",
                          "value": 0
                        }
                      }
                    ]
                  }
                },
                "parameters": {
                  "basic.simultaneous_threshold_milliseconds": 500
                },
                "to": [
                  {
                    "set_variable": {
                      "name": "SpaceFN",
                      "value": 1
                    }
                  },
                  {
                    "key_code": "up_arrow"
                  }
                ],
                "type": "basic"
              },
              {
                "conditions": [
                  {
                    "name": "SpaceFN",
                    "type": "variable_if",
                    "value": 1
                  }
                ],
                "from": {
                  "key_code": "k",
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  }
                },
                "to": [
                  {
                    "key_code": "up_arrow"
                  }
                ],
                "type": "basic"
              },
              {
                "from": {
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  },
                  "simultaneous": [
                    {
                      "key_code": "spacebar"
                    },
                    {
                      "key_code": "h"
                    }
                  ],
                  "simultaneous_options": {
                    "key_down_order": "strict",
                    "key_up_order": "strict_inverse",
                    "to_after_key_up": [
                      {
                        "set_variable": {
                          "name": "SpaceFN",
                          "value": 0
                        }
                      }
                    ]
                  }
                },
                "parameters": {
                  "basic.simultaneous_threshold_milliseconds": 500
                },
                "to": [
                  {
                    "set_variable": {
                      "name": "SpaceFN",
                      "value": 1
                    }
                  },
                  {
                    "key_code": "left_arrow"
                  }
                ],
                "type": "basic"
              },
              {
                "conditions": [
                  {
                    "name": "SpaceFN",
                    "type": "variable_if",
                    "value": 1
                  }
                ],
                "from": {
                  "key_code": "h",
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  }
                },
                "to": [
                  {
                    "key_code": "left_arrow"
                  }
                ],
                "type": "basic"
              },
              {
                "from": {
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  },
                  "simultaneous": [
                    {
                      "key_code": "spacebar"
                    },
                    {
                      "key_code": "l"
                    }
                  ],
                  "simultaneous_options": {
                    "key_down_order": "strict",
                    "key_up_order": "strict_inverse",
                    "to_after_key_up": [
                      {
                        "set_variable": {
                          "name": "SpaceFN",
                          "value": 0
                        }
                      }
                    ]
                  }
                },
                "parameters": {
                  "basic.simultaneous_threshold_milliseconds": 500
                },
                "to": [
                  {
                    "set_variable": {
                      "name": "SpaceFN",
                      "value": 1
                    }
                  },
                  {
                    "key_code": "right_arrow"
                  }
                ],
                "type": "basic"
              },
              {
                "conditions": [
                  {
                    "name": "SpaceFN",
                    "type": "variable_if",
                    "value": 1
                  }
                ],
                "from": {
                  "key_code": "l",
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  }
                },
                "to": [
                  {
                    "key_code": "right_arrow"
                  }
                ],
                "type": "basic"
              }
            ]
          },
          {
            "description": "RCMD is CTRL",
            "manipulators": [
              {
                "from": {
                  "key_code": "right_gui",
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  }
                },
                "to": [
                  {
                    "key_code": "left_control"
                  }
                ],
                "type": "basic"
              }
            ]
          },
          {
            "description": "Capslock is CTRL/Escape",
            "manipulators": [
              {
                "from": {
                  "key_code": "caps_lock",
                  "modifiers": {
                    "optional": [
                      "any"
                    ]
                  }
                },
                "to": [
                  {
                    "key_code": "left_control",
                    "lazy": true
                  }
                ],
                "to_if_alone": [
                  {
                    "key_code": "escape"
                  }
                ],
                "to_if_held_down": [
                  {
                    "key_code": "delete_or_backspace"
                  }
                ],
                "type": "basic"
              }
            ]
          }
        ]
      },
      "devices": [],
      "fn_function_keys": [],
      "name": "Spacey",
      "selected": true,
      "simple_modifications": [],
      "virtual_hid_keyboard": {
        "caps_lock_delay_milliseconds": 0,
        "keyboard_type": "ansi"
      }
    },
    {
      "complex_modifications": {
        "parameters": {
          "basic.simultaneous_threshold_milliseconds": 50,
          "basic.to_delayed_action_delay_milliseconds": 500,
          "basic.to_if_alone_timeout_milliseconds": 1000,
          "basic.to_if_held_down_threshold_milliseconds": 500
        },
        "rules": []
      },
      "devices": [],
      "fn_function_keys": [],
      "name": "Vanilla",
      "selected": false,
      "simple_modifications": [],
      "virtual_hid_keyboard": {
        "caps_lock_delay_milliseconds": 0,
        "keyboard_type": "ansi"
      }
    }
  ]
}