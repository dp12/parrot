Feature: Rotate words
  In order to switch between common variations of a word
  As an Emacs user and parrot lover
  I want to rotate text

  Background:
    Given I switch to buffer "*parrot*"
    And the buffer is empty

  Scenario: Lowercase rotation
    Given I set parrot-dict to ((:rot ("snek" "snake")))
    And I insert:
    """
    this_is_snek_charmer_stawp
    """
    And I go to word "snek"
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    this_is_snake_charmer_stawp
    """
    Then the cursor should be between "is_" and "snake"

  Scenario: Next and previous rotations with wrapping
    Given I set parrot-dict to ((:rot ("shiba-inu" "such_animate" "wow")))
    And I insert:
    """
    shiba-inu
    """
    And I go to word "shiba-inu"
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    such_animate
    """
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    wow
    """
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    shiba-inu
    """
    And I call "parrot-prev-word-at-point"
    Then I should see:
    """
    wow
    """
    And I call "parrot-prev-word-at-point"
    Then I should see:
    """
    such_animate
    """
    And I call "parrot-prev-word-at-point"
    Then I should see:
    """
    shiba-inu
    """

  Scenario: Jump to rotation on the right
    Given I set parrot-dict to ((:rot ("hunt" "stunt")))
    And I insert:
    """
    tehwildhunt
    """
    And I place the cursor between "teh" and "wild"
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    tehwildstunt
    """
    Then the cursor should be between "wild" and "stunt"

  Scenario: Jump to rotation on the left
    Given I set parrot-dict to ((:rot ("teh" "the")))
    And I insert:
    """
    tehwildhunt
    """
    And I place the cursor between "wild" and "hunt"
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    thewildhunt
    """
    Then the cursor should be between "th" and "ewild"

  Scenario: When both left/right matches are found, jump to closest match 
    Given I set parrot-dict to ((:rot ("true" "false")) (:rot ("this" "that")))
    And I insert:
    """
    truehumebugthis
    """
    And I place the cursor between "hum" and "ebug"
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    truehumebugthat
    """
    And I place the cursor between "hume" and "bug"
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    truehumebugthis
    """
    And I place the cursor between "hu" and "mebug"
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    falsehumebugthis
    """

  Scenario: Rotation with capitalized option set
    Given I set parrot-dict to ((:rot ("case" "casey") :caps t))
    And I insert:
    """
    camelCase
    """
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    camelCasey
    """
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    camelCase
    """

  Scenario: Rotation with upcase option set
    Given I set parrot-dict to ((:rot ("peeple" "sheeple") :upcase t))
    And I insert:
    """
    PEEPLE
    """
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    SHEEPLE
    """
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    PEEPLE
    """

  Scenario: Rotation with all options set
    Given I set parrot-dict to ((:rot ("tick" "tock") :caps t :upcase t))
    And I insert:
    """
    tickTickTICK
    """
    And I place the cursor after "ti"
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    tockTickTICK
    """
    And I place the cursor after "Ti"
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    tockTockTICK
    """
    And I place the cursor after "TI"
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    tockTockTOCK
    """

  Scenario: Rotation with lowercase excluded
    Given I set parrot-dict to ((:rot ("yolo" "nolo") :upcase t :lower nil))
    And I insert:
    """
    yoloYOLO
    """
    And I place the cursor after "yo"
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    yoloNOLO
    """

  Scenario: Rotation with increasing case
    Given I set parrot-dict to ((:rot ("wow" "Wow" "WOW")))
    And I insert:
    """
    wow
    """
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    Wow
    """
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    WOW
    """

  Scenario: Rotation with underscores, hyphens, and apostrophes
    Given I set parrot-dict to ((:rot ("is_snek" "isn't-snake")))
    And I insert:
    """
    this_is_snek_stawp
    """
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    this_isn't-snake_stawp
    """
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    this_is_snek_stawp
    """

  Scenario: Rotation with special characters
    Given I set parrot-dict to ((:rot ("!@$#%^" "&*-_=+()")))
    And I insert:
    """
    !@$#%^
    """
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    &*-_=+()
    """
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    !@$#%^
    """

  Scenario: Rotation with numbers
    Given I set parrot-dict to ((:rot ("2" "4" "8" "16")))
    And I insert:
    """
    2
    """
    And I call "parrot-prev-word-at-point"
    Then I should see:
    """
    16
    """
    And I call "parrot-prev-word-at-point"
    Then I should see:
    """
    8
    """
    And I call "parrot-prev-word-at-point"
    Then I should see:
    """
    4
    """

  Scenario: Cursor adjustment with longer to shorter rotation
    Given I set parrot-dict to ((:rot ("false" "true")))
    And I insert:
    """
    This is a false statement
    """
    And I place the cursor after "fals"
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    This is a true statement
    """
    Then the cursor should be after "tru"
    And I clear the buffer
    And I insert:
    """
    This-is-a-false-statement
    """
    And I place the cursor after "state"
    And I call "parrot-next-word-at-point"
    Then I should see:
    """
    This-is-a-true-statement
    """
    Then the cursor should be after "tru"