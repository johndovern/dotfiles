#include QMK_KEYBOARD_H
#include "features/layer_lock.h"

enum custom_keycodes {
    MAC_ZZ = SAFE_RANGE,
    MAC_ZQ,
    MAC_CW,
    LLOCK,
};

// Tap Dance declarations
enum {
    TD_BEG_END,
};

// Tap Dance definitions
tap_dance_action_t tap_dance_actions[] = {
    // Tap once for Escape, twice for Caps Lock
    [TD_BEG_END] = ACTION_TAP_DANCE_DOUBLE(KC_0, KC_DLR),
};

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    if (!process_layer_lock(keycode, record, LLOCK)) { return false; }
    switch (keycode) {
    case MAC_ZZ:
        if (record->event.pressed)
        {
            SEND_STRING("ZZ");
        }
        break;
    case MAC_ZQ:
        if (record->event.pressed)
        {
            SEND_STRING("ZQ");
        }
        break;
    case MAC_CW:
        if (record->event.pressed)
        {
           SEND_STRING(":w\n");
        }
        break;
    }
    return true;
};

// Each layer gets a name for readability, which is then used in the keymap matrix below.
// The underscores don't mean anything - you can have a layer called STUFF or any other name.
// Layer names don't all need to be of the same length, obviously, and you can also skip them
// entirely and just use numbers.
#define _BASE   0
#define _NAV    1
#define _MOUSE  2
#define _BUTTON 3
#define _MEDIA  4
#define _NUM    5
#define _SYM    6
#define _FUN    7
#define _BOOT   8

// Some basic macros
#define TASK   LCTL(LSFT(KC_ESC))
#define TAB_R  LCTL(KC_TAB)
#define TAB_L  LCTL(LSFT(KC_TAB))
#define TAB_RO LCTL(LSFT(KC_T))

// My defs
// MEH Keys
#define MH_TAB  MEH_T(KC_TAB)
#define MH_ENT  MEH_T(KC_ENT)
#define MH_LBRC MEH_T(KC_LBRC)

// Shift Keys
#define LS_ESC  SFT_T(KC_ESC)
#define LS_QOUT SFT_T(KC_QUOT)
#define LS_RBRC SFT_T(KC_RBRC)
#define SHF_BKS SFT_T(KC_BSPC)
#define SH_LBRC S(KC_LBRC)
#define SH_RBRC S(KC_RBRC)
#define ALT_COM A(KC_COMM)

// CTRL Keys
#define CTL_BSL CTL_T(KC_BSLS)
#define CTL_SPC CTL_T(KC_SPC)
#define KC_TMUX C(KC_B)

// Base home row
#define LS_T    SFT_T(KC_T)
#define LS_N    SFT_T(KC_N)
#define GUI_S   GUI_T(KC_S)
#define GUI_E   GUI_T(KC_E)
#define CTL_R   CTL_T(KC_R)
#define CTL_I   CTL_T(KC_I)
#define ALT_A   ALT_T(KC_A)
#define ALT_O   ALT_T(KC_O)
#define MEH_M   MEH_T(KC_M)
#define MEH_G   MEH_T(KC_G)
#define BOOT_Z  LT(_BOOT, KC_Z)
#define BOOT_SL LT(_BOOT, KC_SLSH)

// Base Thumb keys
#define MOU_ESC LT(_MOUSE, KC_ESC)
#define NAV_BCS LT(_NAV, KC_BSPC)
#define MED_TAB LT(_MEDIA, KC_TAB)
#define SYM_ENT LT(_SYM, KC_ENT)
#define NUM_SPC LT(_NUM, KC_SPC)
#define FUN_DEL LT(_FUN, KC_DEL)

// Extra Thumb keys
#define MOU_HM  LT(_MOUSE, TO(_BASE))
#define NAV_CAP LT(_NAV, CW_TOGG)
#define MED_CW  LT(_MEDIA, MAC_CW)
#define SYM_TMX LT(_SYM, KC_TMUX)
#define NUM_ZQ  LT(_NUM, MAC_ZQ)
#define FUN_ZZ  LT(_FUN, MAC_ZZ)

// Nav keys
#define REDO    C(KC_R)
#define CUT     C(KC_X)
#define COPY    C(KC_C)
#define PASTE   C(KC_P)
#define UNDO    C(KC_Z)

// Misc. keys
#define BOOT_HM LT(_BOOT, TO(_BASE))

    /* Blank Layer
        // left hand
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   KC_LGUI,   KC_LALT,   KC_LCTL,   KC_LSFT,   _______,  _______,
        _______,   _______,   _______,   _______,   _______,   _______,
        _______,   _______,   TO(_BASE), QK_BOOT,
                                    _______,  _______,
                                    _______,  _______,
                                    _______,  _______,
        // right hand
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                                     _______,   _______,   _______,   _______,   _______,   _______,
                                                           _______,   _______,   _______,   _______,
             _______, _______,
             _______, _______,
             _______, _______

        // left hand
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   _______,   _______,   _______,   _______,   _______,
        _______,   _______,   _______,   _______,
                                    _______,  _______,
                                    _______,  _______,
                                    _______,  _______,
        // right hand
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   _______,   KC_LSFT,   KC_LCTL,   KC_LALT,   KC_LGUI,   _______,
                                     _______,   _______,   _______,   _______,   _______,   _______,
                                                           QK_BOOT,   TO(_BASE), _______,   _______,
             _______, _______,
             _______, _______,
             _______, _______
    */


const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [_BASE] = LAYOUT_5x7(
        // left hand
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   KC_Q,      KC_W,      KC_F,      KC_P,      KC_B,     _______,
        _______,   ALT_A,     CTL_R,     GUI_S,     LS_T,      MEH_G,    _______,
        _______,   BOOT_Z,    KC_X,      KC_C,      KC_D,      KC_V,
        _______,   _______,   MAC_ZZ,   SH_RBRC,
                                    KC_TMUX, MOU_ESC,
                                    ALT_COM, NAV_BCS,
                             TD(TD_BEG_END), MED_TAB,
        // right hand
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   KC_J,      KC_L,      KC_U,      KC_Y,      KC_QUOT,   _______,
                          _______,   MEH_M,     LS_N,      GUI_E,     CTL_I,     ALT_O,     _______,
                                     KC_K,      KC_H,      KC_COMM,   KC_DOT,    BOOT_SL,   _______,
                                                SH_LBRC,   MAC_ZQ,   _______,   _______,
             SYM_ENT, MOU_HM,
             NUM_SPC, CW_TOGG,
             FUN_DEL, MAC_CW
    ),

    [_NAV] = LAYOUT_5x7(
        // left hand
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   KC_LALT,   KC_LCTL,   KC_LGUI,   KC_LSFT,   KC_MEH,   _______,
        _______,   _______,   _______,   KC_MEH,    LLOCK,     _______,
        _______,   _______,   TO(_BASE), QK_BOOT,
                                    _______,  _______,
                                    _______,  _______,
                                    _______,  _______,
        // right hand
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   REDO,      PASTE,     COPY,      CUT,       UNDO,      _______,
                          _______,   KC_LEFT,   KC_DOWN,   KC_UP,     KC_RGHT,   KC_CAPS,   _______,
                                     KC_HOME,   KC_PGDN,   KC_PGUP,   KC_END,    KC_INS,    _______,
                                                           QK_BOOT,   _______,   _______,   _______,
             KC_DEL,  _______,
             KC_SPC,  _______,
             KC_ENT,  _______
    ),

    [_MOUSE] = LAYOUT_5x7(
        // left hand
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   KC_LALT,   KC_LCTL,   KC_LGUI,   KC_LSFT,   KC_MEH,   _______,
        _______,   _______,   _______,   KC_MEH,    LLOCK,     _______,
        _______,   _______,   TO(_BASE), QK_BOOT,
                                    _______,  _______,
                                    _______,  _______,
                                    _______,  _______,
        // right hand
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   REDO,      PASTE,     COPY,      CUT,       UNDO,      _______,
                          _______,   KC_MS_L,   KC_MS_D,   KC_MS_U,   KC_MS_R,   _______,   _______,
                                     KC_WH_L,   KC_WH_D,   KC_WH_U,   KC_WH_R,   _______,   _______,
                                                           _______,   _______,   _______,   _______,
             KC_BTN2,  _______,
             KC_BTN1,  _______,
             KC_BTN3,  _______
    ),

    [_BUTTON] = LAYOUT_5x7(
        // left hand
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   KC_LALT,   KC_LCTL,   KC_LGUI,   KC_LSFT,   KC_MEH,   _______,
        _______,   _______,   _______,   KC_MEH,    LLOCK,     _______,
        _______,   _______,   TO(_BASE), QK_BOOT,
                                    _______,  _______,
                                    _______,  _______,
                                    _______,  _______,
        // right hand
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   REDO,      PASTE,     COPY,      CUT,       UNDO,      _______,
                          _______,   _______,   KC_LSFT,   KC_LCTL,   KC_LALT,   KC_LGUI,   _______,
                                     REDO,      PASTE,     COPY,      CUT,       UNDO,      _______,
                                                           _______,   _______,   _______,   _______,
             _______, _______,
             _______, _______,
             _______, _______
    ),

    [_MEDIA] = LAYOUT_5x7(
        // left hand
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   KC_LALT,   KC_LCTL,   KC_LGUI,   KC_LSFT,   KC_MEH,   _______,
        _______,   _______,   _______,   KC_MEH,    LLOCK,     _______,
        _______,   _______,   TO(_BASE), QK_BOOT,
                                    _______,  _______,
                                    _______,  _______,
                                    _______,  _______,
        // right hand
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   KC_MPRV,   KC_VOLD,   KC_VOLU,   KC_MNXT,   _______,   _______,
                                     _______,   _______,   _______,   _______,   _______,   _______,
                                                           _______,   _______,   _______,   _______,
             KC_MUTE, _______,
             KC_MPLY, _______,
             KC_MSTP, _______
    ),

    [_NUM] = LAYOUT_5x7(
        // left hand
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   KC_LBRC,   KC_7,      KC_8,      KC_9,      KC_RBRC,  _______,
        _______,   KC_SCLN,   KC_4,      KC_5,      KC_6,      KC_EQL,   _______,
        _______,   KC_GRV,    KC_1,      KC_2,      KC_3,      KC_BSLS,
        _______,   _______,   _______,   _______,
                                    _______,  KC_DOT,
                                    _______,  KC_0,
                                    _______,  KC_MINS,
        // right hand
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   KC_MEH,    KC_LSFT,   KC_LGUI,   KC_LCTL,   KC_LALT,   _______,
                                     _______,   LLOCK,     KC_MEH,    _______,   _______,   _______,
                                                           QK_BOOT,   TO(_BASE), _______,   _______,
             _______, _______,
             _______, _______,
             _______, _______
    ),

    [_SYM] = LAYOUT_5x7(
        // left hand
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   KC_LCBR,   KC_AMPR,   KC_ASTR,   KC_LPRN,   KC_RCBR,  _______,
        _______,   KC_COLN,   KC_DLR,    KC_PERC,   KC_CIRC,   KC_PLUS,  _______,
        _______,   KC_TILD,   KC_EXLM,   KC_AT,     KC_HASH,   KC_PIPE,
        _______,   _______,   _______,   _______,
                                    _______,  KC_LPRN,
                                    _______,  KC_RPRN,
                                    _______,  KC_UNDS,
        // right hand
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   KC_MEH,    KC_LSFT,   KC_LGUI,   KC_LCTL,   KC_LALT,   _______,
                                     _______,   LLOCK,     KC_MEH,    _______,   _______,   _______,
                                                           QK_BOOT,   TO(_BASE), _______,   _______,
             _______, _______,
             _______, _______,
             _______, _______
    ),

    [_FUN] = LAYOUT_5x7(
        // left hand
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   KC_F12,    KC_F7,     KC_F8,     KC_F9,     KC_PSCR,  _______,
        _______,   KC_F11,    KC_F4,     KC_F5,     KC_F6,     KC_SCRL,  _______,
        _______,   KC_F10,    KC_F1,     KC_F2,     KC_F3,     KC_PAUS,
        _______,   _______,   _______,   _______,
                                    _______,  _______,
                                    _______,  KC_BSPC,
                                    _______,  KC_TAB,
        // right hand
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   KC_MEH,    KC_LSFT,   KC_LGUI,   KC_LCTL,   KC_LALT,   _______,
                                     _______,   LLOCK,     KC_MEH,    _______,   _______,   _______,
                                                           QK_BOOT,   TO(_BASE), _______,   _______,
             _______, _______,
             _______, _______,
             _______, _______
    ),

    [_BOOT] = LAYOUT_5x7(
        // left hand
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   _______,   _______,   _______,   _______,   _______,  _______,
        _______,   QK_BOOT,   _______,   _______,   _______,   _______,  _______,
        _______,   _______,   _______,   _______,   _______,   _______,
        _______,   _______,   _______,   QK_BOOT,
                                    _______,  _______,
                                    _______,  _______,
                                    _______,  _______,
        // right hand
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   _______,   _______,   _______,   _______,   _______,   _______,
                          _______,   _______,   _______,   _______,   _______,   QK_BOOT,   _______,
                                     _______,   _______,   _______,   _______,   _______,   _______,
                                                           QK_BOOT,   _______,   _______,   _______,
             _______, _______,
             _______, _______,
             _______, _______
    ),
};

