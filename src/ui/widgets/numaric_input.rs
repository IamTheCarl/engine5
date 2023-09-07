//! Numeric inputs enable the user to input numbers.
//! Numbers can be unsigned integers, integers, positive floating point numbers, or real floating point numbers.

use anyhow::{anyhow, Result};
use bevy::{ecs::system::EntityCommands, input::mouse::MouseWheel, prelude::*};
use bevy_ui_navigation::{
    menu::NavMarker,
    prelude::{Focusable, Focused, MenuBuilder, MenuSetting, NavEvent, NavRequest},
    NavRequestSystem,
};
use std::fmt::Write;

use crate::ui::{sub_menus::OverlayMenu, Clipboard};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Sign {
    Posative,
    Negative,
}

impl Default for Sign {
    fn default() -> Self {
        Self::Posative
    }
}

impl std::fmt::Display for Sign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Posative => write!(f, "+"),
            Self::Negative => write!(f, "-"),
        }
    }
}

#[derive(Debug, Clone, Component)]
pub struct NumaricInput {
    /// The sign of the value, can be positive or negative.
    /// Is ignored when converting to unsigned integers.
    sign: Sign,

    // Starts with least significant value, ends with most significant value.
    integer: Vec<u8>,
    fractional: Vec<u8>,
}

impl NumaricInput {
    pub fn new<const INTEGER_DIGITS: usize, const FRACTIONAL_DIGITS: usize>(
        integer: isize,
        fractional: usize,
    ) -> Result<Self> {
        use nobcd::BcdNumber;

        fn nibble_iterator(byte: &u8) -> std::iter::FromFn<impl FnMut() -> Option<u8>> {
            enum State {
                First,
                Second,
                Done,
            }

            let first_nibble = (*byte >> 4) & 0x0F;
            let second_nibble = *byte & 0x0F;
            let mut state = State::First;

            std::iter::from_fn(move || match state {
                State::First => {
                    state = State::Second;
                    Some(first_nibble)
                }
                State::Second => {
                    state = State::Done;
                    Some(second_nibble)
                }
                State::Done => None,
            })
        }

        let sign = if integer >= 0 {
            Sign::Posative
        } else {
            Sign::Negative
        };

        let bcd = BcdNumber::<INTEGER_DIGITS>::new(integer.unsigned_abs())
            .map_err(|_error| anyhow!("Failed to encode integer to BCD."))?;
        let value_integer = bcd
            .bcd_bytes()
            .iter()
            .flat_map(nibble_iterator)
            .skip(INTEGER_DIGITS)
            .collect();

        let bcd = BcdNumber::<FRACTIONAL_DIGITS>::new(fractional)
            .map_err(|_error| anyhow!("Failed to encode fraction to BCD."))?;
        let value_fractional = bcd
            .bcd_bytes()
            .iter()
            .flat_map(nibble_iterator)
            .skip(FRACTIONAL_DIGITS)
            .collect();

        Ok(Self {
            sign,
            integer: value_integer,
            fractional: value_fractional,
        })
    }

    fn is_zero(&self) -> bool {
        for digit in self.integer.iter() {
            if *digit != 0 {
                return false;
            }
        }

        for digit in self.fractional.iter() {
            if *digit != 0 {
                return false;
            }
        }

        true
    }

    fn copy(&mut self, other: &Self) {
        self.integer.iter_mut().for_each(|value| *value = 0);
        self.integer
            .iter_mut()
            .rev()
            .zip(other.integer.iter().rev())
            .for_each(|(ours, theirs)| *ours = *theirs);

        self.fractional.iter_mut().for_each(|value| *value = 0);
        self.fractional
            .iter_mut()
            .zip(other.fractional.iter())
            .for_each(|(ours, theirs)| *ours = *theirs);
    }
}

impl std::cmp::Eq for NumaricInput {}

impl std::cmp::PartialEq for NumaricInput {
    fn eq(&self, other: &Self) -> bool {
        matches!(self.cmp(other), std::cmp::Ordering::Equal)
    }
}

impl std::cmp::Ord for NumaricInput {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering;

        fn compare_digits(
            ours: impl Iterator<Item = u8>,
            theirs: impl Iterator<Item = u8>,
        ) -> Ordering {
            for (ours, theirs) in ours.zip(theirs) {
                let cmp = ours.cmp(&theirs);

                match cmp {
                    // One of them has an order of magnitude on the other.
                    Ordering::Less | Ordering::Greater => return cmp,
                    Ordering::Equal => {
                        // Continue to the next most significant number.
                    }
                }
            }

            // They're all equal.
            Ordering::Equal
        }

        fn compare_integer_slices(ours: &[u8], theirs: &[u8]) -> Ordering {
            let max_length = ours.len().max(theirs.len());
            let ours = std::iter::repeat(0u8)
                .take(max_length - ours.len())
                .chain(ours.iter().copied());

            let theirs = std::iter::repeat(0u8)
                .take(max_length - theirs.len())
                .chain(theirs.iter().copied());

            compare_digits(ours, theirs)
        }

        fn compare_fractional_slices(ours: &[u8], theirs: &[u8]) -> Ordering {
            let max_length = ours.len().max(theirs.len());
            let ours = ours
                .iter()
                .copied()
                .chain(std::iter::repeat(0u8).take(max_length - ours.len()));

            let theirs = theirs
                .iter()
                .copied()
                .chain(std::iter::repeat(0u8).take(max_length - theirs.len()));

            compare_digits(ours, theirs)
        }

        if self.sign == other.sign {
            // How does the integer part compare?
            let integer_cmp = compare_integer_slices(&self.integer, &other.integer);

            match integer_cmp {
                Ordering::Less | Ordering::Greater => integer_cmp,
                Ordering::Equal => {
                    // Use the fractional part as a tie breaker.
                    compare_fractional_slices(&self.fractional, &other.fractional)
                }
            }
        } else if self.is_zero() && other.is_zero() {
            Ordering::Equal
        } else if self.sign == Sign::Posative {
            Ordering::Greater
        } else {
            Ordering::Less
        }
    }
}
impl std::cmp::PartialOrd for NumaricInput {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[test]
fn compare_numaric_inputs() {
    assert_eq!(
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![0],
            fractional: vec![]
        },
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![0],
            fractional: vec![]
        }
    );

    assert_eq!(
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![0, 0],
            fractional: vec![]
        },
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![0],
            fractional: vec![]
        }
    );

    assert_eq!(
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![0, 0],
            fractional: vec![0, 0]
        },
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![0],
            fractional: vec![]
        }
    );

    assert!(
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![1],
            fractional: vec![]
        } > NumaricInput {
            sign: Sign::Posative,
            integer: vec![0],
            fractional: vec![]
        }
    );

    assert_eq!(
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![0],
            fractional: vec![]
        },
        NumaricInput {
            sign: Sign::Negative,
            integer: vec![0],
            fractional: vec![]
        }
    );

    assert!(
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![1],
            fractional: vec![]
        } > NumaricInput {
            sign: Sign::Negative,
            integer: vec![1],
            fractional: vec![]
        }
    );

    assert!(
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![1, 0],
            fractional: vec![]
        } > NumaricInput {
            sign: Sign::Negative,
            integer: vec![3, 0],
            fractional: vec![]
        }
    );

    assert_eq!(
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![1],
            fractional: vec![]
        },
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![1],
            fractional: vec![0]
        }
    );

    assert!(
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![1, 0],
            fractional: vec![1]
        } > NumaricInput {
            sign: Sign::Negative,
            integer: vec![1, 0],
            fractional: vec![]
        }
    );
}

impl ToString for NumaricInput {
    fn to_string(&self) -> String {
        let mut value = String::new();

        if Sign::Negative == self.sign {
            value += "-";
        }

        let mut integer_iterator = self.integer.iter().peekable();

        while integer_iterator.peek() == Some(&&0u8) {
            integer_iterator.next();
        }

        if integer_iterator.peek().is_none() {
            value = "0".into();
        } else {
            for digit in integer_iterator {
                write!(&mut value, "{:01}", digit).ok();
            }
        }

        if !self.fractional.is_empty() {
            write!(&mut value, ".").ok();

            for digit in self.fractional.iter() {
                write!(&mut value, "{:01}", digit).ok();
            }
        }

        value
    }
}

#[test]
fn string_formatting() {
    assert_eq!(
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![0],
            fractional: vec![]
        }
        .to_string(),
        "0"
    );

    assert_eq!(
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![0, 0],
            fractional: vec![]
        }
        .to_string(),
        "0"
    );

    assert_eq!(
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![0],
            fractional: vec![0, 0]
        }
        .to_string(),
        "0.00"
    );

    assert_eq!(
        NumaricInput {
            sign: Sign::Negative,
            integer: vec![0],
            fractional: vec![]
        }
        .to_string(),
        "0"
    );

    assert_eq!(
        NumaricInput {
            sign: Sign::Negative,
            integer: vec![1],
            fractional: vec![]
        }
        .to_string(),
        "-1"
    );

    assert_eq!(
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![0, 0, 1],
            fractional: vec![]
        }
        .to_string(),
        "1"
    );
}

impl NumaricInput {
    #[allow(unused)]
    pub fn as_unsigned<I>(&self) -> I
    where
        I: TryFrom<usize> + std::fmt::Debug,
    {
        let mut value = 0;

        for (index, digit) in self.integer.iter().rev().enumerate() {
            let exponent = 10usize.pow(index as u32);
            value += *digit as usize * exponent;
        }

        if let Ok(value) = value.try_into() {
            value
        } else {
            panic!("Source integer is out of range.")
        }
    }

    #[allow(unused)]
    pub fn as_signed<I>(&self) -> I
    where
        I: TryFrom<isize> + std::fmt::Debug,
    {
        let mut value = 0;

        for (index, digit) in self.integer.iter().rev().enumerate() {
            let exponent = 10isize.pow(index as u32);
            value += *digit as isize * exponent;
        }

        value *= match self.sign {
            Sign::Posative => 1,
            Sign::Negative => -1,
        };

        if let Ok(value) = value.try_into() {
            value
        } else {
            panic!("Source integer is out of range.")
        }
    }

    #[allow(unused)]
    pub fn as_f64(&self) -> f64 {
        let mut value = 0.0;

        for (index, digit) in self.integer.iter().rev().enumerate() {
            let exponent = 10f64.powi(index as i32);
            value += *digit as f64 * exponent;
        }

        for (index, digit) in self.fractional.iter().enumerate() {
            let exponent = 10f64.powi(index as i32) * 10f64;
            value += *digit as f64 / exponent;
        }

        value *= match self.sign {
            Sign::Posative => 1.0,
            Sign::Negative => -1.0,
        };

        value
    }

    fn update_from_parts(&mut self, sign: Sign, integer: &str, fractional: &str) {
        self.sign = sign;

        // Clear the number out first.
        self.integer.iter_mut().for_each(|value| *value = 0);

        self.integer
            .iter_mut()
            .rev()
            .zip(integer.chars().rev())
            .for_each(|(target, source)| {
                *target = source.to_digit(10).unwrap() as u8;
            });

        // Do it again for the fractional part.
        self.fractional.iter_mut().for_each(|value| *value = 0);

        self.fractional
            .iter_mut()
            .zip(fractional.chars())
            .for_each(|(target, source)| {
                *target = source.to_digit(10).unwrap() as u8;
            });
    }
}

#[test]
fn numaric_input() {
    let value = NumaricInput {
        sign: Sign::Negative,
        integer: vec![1, 2, 3],
        fractional: vec![1, 2, 3],
    };

    assert_eq!(value.as_unsigned::<u8>(), 123);
    assert_eq!(value.as_unsigned::<u16>(), 123);
    assert_eq!(value.as_unsigned::<u32>(), 123);
    assert_eq!(value.as_unsigned::<u64>(), 123);

    assert_eq!(value.as_signed::<i8>(), -123);
    assert_eq!(value.as_signed::<i16>(), -123);
    assert_eq!(value.as_signed::<i32>(), -123);
    assert_eq!(value.as_signed::<i64>(), -123);

    assert!((value.as_f64() - -123.123) < 0.001);
}

#[test]
fn update_from_parts() {
    let mut value = NumaricInput {
        sign: Sign::Posative,
        integer: vec![0, 0],
        fractional: vec![0, 0],
    };

    value.update_from_parts(Sign::Posative, "1", "2");
    assert_eq!(
        value,
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![0, 1],
            fractional: vec![2, 0],
        }
    );

    value.update_from_parts(Sign::Posative, "123", "4");
    assert_eq!(
        value,
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![2, 3],
            fractional: vec![4, 0],
        }
    );

    value.update_from_parts(Sign::Posative, "1", "234");
    assert_eq!(
        value,
        NumaricInput {
            sign: Sign::Posative,
            integer: vec![0, 1],
            fractional: vec![2, 3],
        }
    );

    value.update_from_parts(Sign::Negative, "1", "2");
    assert_eq!(
        value,
        NumaricInput {
            sign: Sign::Negative,
            integer: vec![0, 1],
            fractional: vec![2, 0],
        }
    );
}

/// Spawn a numric input.
/// # Arguments:
/// * `commands` - Bevy entity commands used to spawn the entity.
/// * `label` - The text to label this number with.
/// * `sign` - If provided, the sign of the initial value. If not provided, only positive values are permitted.
/// * `initial_integer` - the initial digits of an integer part of the value. If empty, there will not be an integer part of the number. The user will only be able to change the digits already defined, so this a way to set your range and precision limits.
/// * `initial_fraction`- the initial digits oft he fractional part of the value. If empty, there will not be a fractional part of the number. The user will only be able to change the digits already defined, so this a way to set your range and precision limits.
pub fn spawn_numaric_input<'w, 's, 'a>(
    commands: &'a mut Commands<'w, 's>,
    label: &str,
    unit: &str,
    permit_negative: bool,
    initial_value: NumaricInput,
) -> EntityCommands<'w, 's, 'a> {
    let button_entity = commands
        .spawn((
            ButtonBundle {
                style: Style {
                    justify_content: JustifyContent::Center,
                    align_items: AlignItems::Center,
                    width: Val::Percent(100.0),
                    ..Default::default()
                },
                background_color: Color::DARK_GRAY.into(),
                ..Default::default()
            },
            Focusable::new(),
        ))
        .id();

    let numaric_input_entity = commands
        .spawn((NodeBundle {
            style: Style {
                display: Display::Flex,
                width: Val::Percent(100.0),
                justify_content: JustifyContent::SpaceBetween,
                flex_direction: FlexDirection::Row,
                ..Default::default()
            },
            ..Default::default()
        },))
        .add_child(button_entity)
        .id();

    commands.entity(button_entity).with_children(|parent| {
        parent
            .spawn((NodeBundle {
                style: Style {
                    display: Display::Flex,
                    width: Val::Percent(50.0),
                    justify_content: JustifyContent::SpaceBetween,
                    flex_direction: FlexDirection::Row,
                    ..Default::default()
                },
                ..Default::default()
            },))
            .with_children(|parent| {
                parent.spawn(TextBundle::from_section(
                    format!("{}:", label),
                    TextStyle {
                        font_size: 40.0,
                        color: Color::WHITE,
                        ..Default::default()
                    },
                ));

                parent
                    .spawn((
                        NodeBundle {
                            style: Style {
                                display: Display::Flex,
                                justify_content: JustifyContent::SpaceBetween,
                                flex_direction: FlexDirection::Row,
                                ..Default::default()
                            },
                            ..Default::default()
                        },
                        MenuSetting::new(),
                        MenuBuilder::EntityParent(button_entity),
                        NavMarker(OverlayMenu),
                    ))
                    .with_children(|parent| {
                        if permit_negative {
                            parent
                                .spawn((
                                    ButtonBundle {
                                        style: Style {
                                            justify_content: JustifyContent::Center,
                                            align_items: AlignItems::Center,
                                            width: Val::Percent(100.0),
                                            ..Default::default()
                                        },
                                        background_color: Color::DARK_GRAY.into(),
                                        ..Default::default()
                                    },
                                    SignReference {
                                        numaric_input_entity,
                                    },
                                    Focusable::new(),
                                ))
                                .with_children(|parent| {
                                    parent.spawn((
                                        TextBundle::from_section(
                                            format!("{}", initial_value.sign),
                                            TextStyle {
                                                font_size: 40.0,
                                                color: Color::WHITE,
                                                ..Default::default()
                                            },
                                        ),
                                        SignReference {
                                            numaric_input_entity,
                                        },
                                    ));
                                });
                        }

                        for (index, digit) in initial_value.integer.iter().enumerate() {
                            parent
                                .spawn((
                                    ButtonBundle {
                                        style: Style {
                                            justify_content: JustifyContent::Center,
                                            align_items: AlignItems::Center,
                                            width: Val::Percent(100.0),
                                            ..Default::default()
                                        },
                                        background_color: Color::DARK_GRAY.into(),
                                        ..Default::default()
                                    },
                                    IntegerDigit,
                                    DigitReference {
                                        numaric_input_entity,
                                        index,
                                    },
                                    Focusable::new(),
                                ))
                                .with_children(|parent| {
                                    parent.spawn((
                                        TextBundle::from_section(
                                            format!("{:01}", digit),
                                            TextStyle {
                                                font_size: 40.0,
                                                color: Color::WHITE,
                                                ..Default::default()
                                            },
                                        ),
                                        IntegerDigit,
                                        DigitReference {
                                            numaric_input_entity,
                                            index,
                                        },
                                    ));
                                });
                        }

                        if !initial_value.fractional.is_empty() {
                            parent.spawn(TextBundle::from_section(
                                ".",
                                TextStyle {
                                    font_size: 40.0,
                                    color: Color::WHITE,
                                    ..Default::default()
                                },
                            ));

                            for (index, digit) in initial_value.fractional.iter().enumerate() {
                                parent
                                    .spawn((
                                        ButtonBundle {
                                            style: Style {
                                                justify_content: JustifyContent::Center,
                                                align_items: AlignItems::Center,
                                                width: Val::Percent(100.0),
                                                ..Default::default()
                                            },
                                            background_color: Color::DARK_GRAY.into(),
                                            ..Default::default()
                                        },
                                        FractionalDigit,
                                        DigitReference {
                                            numaric_input_entity,
                                            index,
                                        },
                                        Focusable::new(),
                                    ))
                                    .with_children(|parent| {
                                        parent.spawn((
                                            TextBundle::from_section(
                                                format!("{:01}", digit),
                                                TextStyle {
                                                    font_size: 40.0,
                                                    color: Color::WHITE,
                                                    ..Default::default()
                                                },
                                            ),
                                            FractionalDigit,
                                            DigitReference {
                                                numaric_input_entity,
                                                index,
                                            },
                                        ));
                                    });
                            }
                        }

                        parent.spawn(TextBundle::from_section(
                            unit,
                            TextStyle {
                                font_size: 40.0,
                                color: Color::WHITE,
                                ..Default::default()
                            },
                        ));
                    });
            });
    });

    let mut entity_commands = commands.entity(numaric_input_entity);
    entity_commands.insert(initial_value);

    entity_commands
}

#[derive(Component)]
struct IntegerDigit;

#[derive(Component)]
struct FractionalDigit;

#[derive(Component)]
struct DigitReference {
    numaric_input_entity: Entity,
    index: usize,
}

#[derive(Component)]
struct SignReference {
    numaric_input_entity: Entity,
}

#[allow(clippy::complexity)]
fn update_digit_text(
    numaric_entities: Query<&NumaricInput, Changed<NumaricInput>>,
    mut integer_digits: Query<
        (&mut Text, &DigitReference),
        (
            With<IntegerDigit>,
            Without<FractionalDigit>,
            Without<SignReference>,
        ),
    >,
    mut fractional_digits: Query<
        (&mut Text, &DigitReference),
        (
            With<FractionalDigit>,
            Without<IntegerDigit>,
            Without<SignReference>,
        ),
    >,
    mut signs: Query<(&mut Text, &SignReference)>,
) {
    for (mut text, integer_digit) in integer_digits.iter_mut() {
        if let Ok(numaric_input) = numaric_entities.get(integer_digit.numaric_input_entity) {
            let value = numaric_input.integer[integer_digit.index];
            text.sections[0].value = format!("{:01}", value);
        }
    }

    for (mut text, fractional_digit) in fractional_digits.iter_mut() {
        if let Ok(numaric_input) = numaric_entities.get(fractional_digit.numaric_input_entity) {
            let value = numaric_input.fractional[fractional_digit.index];
            text.sections[0].value = format!("{:01}", value);
        }
    }

    for (mut text, sign_reference) in signs.iter_mut() {
        if let Ok(numaric_input) = numaric_entities.get(sign_reference.numaric_input_entity) {
            text.sections[0].value = format!("{}", numaric_input.sign);
        }
    }
}

fn get_numaric_input<'a, R>(
    integer_digits: impl Iterator<Item = &'a DigitReference>,
    fractional_digits: impl Iterator<Item = &'a DigitReference>,
    signs: impl Iterator<Item = &'a SignReference>,
    numaric_entities: &Query<&NumaricInput>,

    func: impl FnOnce(&NumaricInput) -> R,
) -> Option<R> {
    for integer_digit in integer_digits {
        if let Ok(numaric_input) = numaric_entities.get(integer_digit.numaric_input_entity) {
            return Some(func(numaric_input));
        }
    }

    for fraction_digit in fractional_digits {
        if let Ok(numaric_input) = numaric_entities.get(fraction_digit.numaric_input_entity) {
            return Some(func(numaric_input));
        }
    }

    for sign in signs {
        if let Ok(numaric_input) = numaric_entities.get(sign.numaric_input_entity) {
            return Some(func(numaric_input));
        }
    }

    None
}

fn get_numaric_input_mut<'a, R>(
    integer_digits: impl Iterator<Item = &'a DigitReference>,
    fractional_digits: impl Iterator<Item = &'a DigitReference>,
    signs: impl Iterator<Item = &'a SignReference>,
    numaric_entities: &mut Query<&mut NumaricInput>,

    func: impl FnOnce(Mut<NumaricInput>) -> R,
) -> Option<R> {
    for integer_digit in integer_digits {
        if let Ok(numaric_input) = numaric_entities.get_mut(integer_digit.numaric_input_entity) {
            return Some(func(numaric_input));
        }
    }

    for fraction_digit in fractional_digits {
        if let Ok(numaric_input) = numaric_entities.get_mut(fraction_digit.numaric_input_entity) {
            return Some(func(numaric_input));
        }
    }

    for sign in signs {
        if let Ok(numaric_input) = numaric_entities.get_mut(sign.numaric_input_entity) {
            return Some(func(numaric_input));
        }
    }

    None
}

fn get_digit_mut<'a>(
    integer_digits: impl Iterator<Item = &'a DigitReference>,
    fractional_digits: impl Iterator<Item = &'a DigitReference>,
    numaric_entities: &mut Query<&mut NumaricInput>,

    func: impl Fn(&mut u8),
) {
    for integer_digit in integer_digits {
        if let Ok(mut numaric_input) = numaric_entities.get_mut(integer_digit.numaric_input_entity)
        {
            func(&mut numaric_input.integer[integer_digit.index])
        }
    }

    for fraction_digit in fractional_digits {
        if let Ok(mut numaric_input) = numaric_entities.get_mut(fraction_digit.numaric_input_entity)
        {
            func(&mut numaric_input.fractional[fraction_digit.index])
        }
    }
}

fn increment_digit<'a>(
    integer_digits: impl Iterator<Item = &'a DigitReference>,
    fractional_digits: impl Iterator<Item = &'a DigitReference>,
    numaric_entities: &mut Query<&mut NumaricInput>,
) {
    get_digit_mut(
        integer_digits,
        fractional_digits,
        numaric_entities,
        |value| {
            *value += 1;
            *value %= 10;
        },
    );
}

fn decrement_digit<'a>(
    integer_digits: impl Iterator<Item = &'a DigitReference>,
    fractional_digits: impl Iterator<Item = &'a DigitReference>,
    numaric_entities: &mut Query<&mut NumaricInput>,
) {
    get_digit_mut(
        integer_digits,
        fractional_digits,
        numaric_entities,
        |value| {
            *value = value.checked_sub(1).unwrap_or(9);
        },
    );
}

fn get_sign_mut<'a>(
    signs: impl Iterator<Item = &'a SignReference>,
    numaric_entities: &mut Query<&mut NumaricInput>,

    func: impl Fn(&mut Sign),
) {
    for sign in signs {
        if let Ok(mut numaric_input) = numaric_entities.get_mut(sign.numaric_input_entity) {
            func(&mut numaric_input.sign)
        }
    }
}

fn set_posative<'a>(
    signs: impl Iterator<Item = &'a SignReference>,
    numaric_entities: &mut Query<&mut NumaricInput>,
) {
    get_sign_mut(signs, numaric_entities, |sign| *sign = Sign::Posative);
}

fn set_negative<'a>(
    signs: impl Iterator<Item = &'a SignReference>,
    numaric_entities: &mut Query<&mut NumaricInput>,
) {
    get_sign_mut(signs, numaric_entities, |sign| *sign = Sign::Negative);
}

#[allow(clippy::complexity)]
fn process_digit_general_input(
    mut nav_requests: EventWriter<NavRequest>,
    mut nav_events: EventReader<NavEvent>,

    integer_digits: Query<
        &DigitReference,
        (With<Focused>, With<IntegerDigit>, Without<FractionalDigit>),
    >,
    fractional_digits: Query<
        &DigitReference,
        (With<Focused>, With<FractionalDigit>, Without<IntegerDigit>),
    >,
    signs: Query<&SignReference, With<Focused>>,

    mut numaric_entities: Query<&mut NumaricInput>,
) {
    for event in nav_events.iter() {
        if let NavEvent::NoChanges { from: _, request } = event {
            match request {
                NavRequest::Action => {
                    if !integer_digits.is_empty()
                        || !fractional_digits.is_empty()
                        || !signs.is_empty()
                    {
                        nav_requests.send(NavRequest::Cancel);
                    }
                }
                NavRequest::Move(direction) => match direction {
                    bevy_ui_navigation::events::Direction::North => {
                        increment_digit(
                            integer_digits.iter(),
                            fractional_digits.iter(),
                            &mut numaric_entities,
                        );
                        set_posative(signs.iter(), &mut numaric_entities);
                    }
                    bevy_ui_navigation::events::Direction::South => {
                        decrement_digit(
                            integer_digits.iter(),
                            fractional_digits.iter(),
                            &mut numaric_entities,
                        );
                        set_negative(signs.iter(), &mut numaric_entities);
                    }
                    _ => {}
                },
                _ => {}
            }
        }
    }
}

#[allow(clippy::complexity)]
fn process_digit_mouse_input(
    mut mouse_wheel_events: EventReader<MouseWheel>,

    integer_digits: Query<
        &DigitReference,
        (With<Focused>, With<IntegerDigit>, Without<FractionalDigit>),
    >,
    fractional_digits: Query<
        &DigitReference,
        (With<Focused>, With<FractionalDigit>, Without<IntegerDigit>),
    >,
    signs: Query<&SignReference, With<Focused>>,

    mut numaric_entities: Query<&mut NumaricInput>,
) {
    for event in mouse_wheel_events.iter() {
        let value = event.y;

        if value.abs() > 0.1 {
            if value.is_sign_positive() {
                increment_digit(
                    integer_digits.iter(),
                    fractional_digits.iter(),
                    &mut numaric_entities,
                );
                set_posative(signs.iter(), &mut numaric_entities);
            } else {
                decrement_digit(
                    integer_digits.iter(),
                    fractional_digits.iter(),
                    &mut numaric_entities,
                );
                set_negative(signs.iter(), &mut numaric_entities);
            }
        }
    }
}

#[allow(clippy::complexity)]
fn process_digit_keyboard_input(
    mut nav_requests: EventWriter<NavRequest>,
    mut events: EventReader<ReceivedCharacter>,

    integer_digits: Query<
        &DigitReference,
        (With<Focused>, With<IntegerDigit>, Without<FractionalDigit>),
    >,
    fractional_digits: Query<
        &DigitReference,
        (With<Focused>, With<FractionalDigit>, Without<IntegerDigit>),
    >,
    signs: Query<&SignReference, With<Focused>>,

    mut numaric_entities: Query<&mut NumaricInput>,
) {
    for event in events.iter() {
        if let Some(number) = event.char.to_digit(10) {
            get_digit_mut(
                integer_digits.iter(),
                fractional_digits.iter(),
                &mut numaric_entities,
                |value| {
                    *value = number as u8;
                },
            );

            nav_requests.send(NavRequest::Move(
                bevy_ui_navigation::events::Direction::West,
            ));
        } else {
            match event.char {
                '+' => set_posative(signs.iter(), &mut numaric_entities),
                '-' => set_negative(signs.iter(), &mut numaric_entities),
                _ => {}
            }
        }
    }
}

fn parse_number(input: &str) -> nom::IResult<&str, (Sign, &str, &str)> {
    use nom::{
        branch::alt,
        character::complete::{char as nom_char, digit0, multispace0},
        combinator::{success, value},
        sequence::{delimited, pair, separated_pair, terminated},
        Parser,
    };

    let (input, (sign, (integer, fractional))) = delimited(
        multispace0,
        pair(
            terminated(
                alt((
                    value(Sign::Posative, nom_char('+')),
                    value(Sign::Negative, nom_char('-')),
                ))
                .or(success(Sign::Posative)),
                multispace0,
            ),
            separated_pair(
                digit0,
                delimited(multispace0, nom_char('.'), multispace0),
                digit0,
            ),
        ),
        multispace0,
    )(input)?;

    Ok((input, (sign, integer, fractional)))
}

#[test]
fn number_parsing() {
    assert_eq!(
        parse_number("+123.456"),
        Ok(("", (Sign::Posative, "123", "456")))
    );
    assert_eq!(
        parse_number("+ 123 . 456 "),
        Ok(("", (Sign::Posative, "123", "456")))
    );
    assert_eq!(
        parse_number("123.456"),
        Ok(("", (Sign::Posative, "123", "456")))
    );
    assert_eq!(
        parse_number("-123.456"),
        Ok(("", (Sign::Negative, "123", "456")))
    );
}

#[allow(clippy::complexity)]
fn copy(
    keyboard_status: Res<Input<KeyCode>>,

    numaric_entities: Query<&NumaricInput>,
    integer_digits: Query<
        &DigitReference,
        (With<Focused>, With<IntegerDigit>, Without<FractionalDigit>),
    >,
    fractional_digits: Query<
        &DigitReference,
        (With<Focused>, With<FractionalDigit>, Without<IntegerDigit>),
    >,
    signs: Query<&SignReference, With<Focused>>,
    mut clipboard: ResMut<Clipboard>,
) {
    if keyboard_status.pressed(KeyCode::ControlLeft) && keyboard_status.just_pressed(KeyCode::C) {
        get_numaric_input(
            integer_digits.iter(),
            fractional_digits.iter(),
            signs.iter(),
            &numaric_entities,
            |numaric_input| {
                match clipboard.set_contents(numaric_input.to_string()) {
                    Ok(()) => {
                        // Linux requires this, for some reason...
                        // https://github.com/alacritty/copypasta/issues/49
                        clipboard.get_contents().ok();
                    }
                    Err(error) => {
                        log::error!("Failed to copy number to system clipboard: {:?}", error);
                    }
                }
            },
        );
    }
}

#[allow(clippy::complexity)]
fn paste(
    keyboard_status: Res<Input<KeyCode>>,

    mut numaric_entities: Query<&mut NumaricInput>,
    integer_digits: Query<
        &DigitReference,
        (With<Focused>, With<IntegerDigit>, Without<FractionalDigit>),
    >,
    fractional_digits: Query<
        &DigitReference,
        (With<Focused>, With<FractionalDigit>, Without<IntegerDigit>),
    >,
    signs: Query<&SignReference, With<Focused>>,
    mut clipboard: ResMut<Clipboard>,
) {
    if keyboard_status.pressed(KeyCode::ControlLeft) && keyboard_status.just_pressed(KeyCode::V) {
        get_numaric_input_mut(
            integer_digits.iter(),
            fractional_digits.iter(),
            signs.iter(),
            &mut numaric_entities,
            |mut numaric_input| match clipboard.get_contents() {
                Ok(content) => {
                    if let Ok((_leftovers, (sign, integer, fractional))) = parse_number(&content) {
                        numaric_input.update_from_parts(sign, integer, fractional);
                    } else {
                        log::error!("Failed to parse number from system clipboard.");
                    }
                }
                Err(error) => {
                    log::error!("Failed to paste number from system clipboard: {:?}", error);
                }
            },
        );
    }
}

#[derive(Debug, Component)]
pub struct RangeLimit {
    pub start: std::ops::Bound<NumaricInput>,
    pub end: std::ops::Bound<NumaricInput>,
}

impl RangeLimit {
    pub fn new(range: impl std::ops::RangeBounds<NumaricInput>) -> Self {
        Self {
            start: range.start_bound().cloned(),
            end: range.end_bound().cloned(),
        }
    }
}

impl std::ops::RangeBounds<NumaricInput> for RangeLimit {
    fn start_bound(&self) -> std::ops::Bound<&NumaricInput> {
        self.start.as_ref()
    }

    fn end_bound(&self) -> std::ops::Bound<&NumaricInput> {
        self.end.as_ref()
    }
}

fn enforce_range_bounds(mut numaric_inputs: Query<(&mut NumaricInput, &RangeLimit)>) {
    for (mut input, limits) in numaric_inputs.iter_mut() {
        // Excluded bounds aren't properly enforced because I'd have to add support for arithmetic
        // operations on numaric inputs and I've already packed enough work into this thing.

        match &limits.start {
            std::ops::Bound::Included(limit) | std::ops::Bound::Excluded(limit) => {
                if *input < *limit {
                    input.copy(limit);
                }
            }
            std::ops::Bound::Unbounded => {
                // Nothing to do about that.
            }
        }

        match &limits.end {
            std::ops::Bound::Included(limit) | std::ops::Bound::Excluded(limit) => {
                if *input > *limit {
                    input.copy(limit);
                }
            }
            std::ops::Bound::Unbounded => {
                // Nothing to do about that.
            }
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
struct NumaricalInputSet;

pub fn setup(app: &mut App) {
    app.configure_set(Update, NumaricalInputSet.after(NavRequestSystem));
    app.add_systems(
        Update,
        (
            // Take all our inputs.
            process_digit_general_input.in_set(NumaricalInputSet),
            process_digit_mouse_input.in_set(NumaricalInputSet),
            process_digit_keyboard_input.in_set(NumaricalInputSet),
            paste.in_set(NumaricalInputSet),
            // Enforce range bounds.
            enforce_range_bounds.after(NumaricalInputSet),
            // Now we can display/copy.
            update_digit_text.after(enforce_range_bounds),
            copy.after(enforce_range_bounds),
        ),
    );
}
