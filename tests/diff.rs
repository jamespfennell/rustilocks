use assert_cmd::prelude::*;
use std::process;

fn run_diff_test(path: &str) {
    let reference = run(process::Command::new("tests/craftinginterpreters/clox").arg(path));
    let rustilocks = run(process::Command::cargo_bin("rustilocks")
        .expect("failed to build rustilocks")
        .arg("run")
        .arg(path));
    assert_eq!(reference, rustilocks);
}

#[derive(Debug, PartialEq, Eq, Default)]
struct Result {
    stdout: String,
    stderr: String,
    exit_code: i32,
}

fn run(cmd: &mut process::Command) -> Result {
    let output = cmd
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::piped())
        .spawn()
        .expect("failed to spawn the reference command")
        .wait_with_output()
        .expect("ran the command");
    Result {
        stdout: String::from_utf8(output.stdout).expect("failed to convert stdout to utf8"),
        stderr: String::from_utf8(output.stderr).expect("failed to convert stdout to utf8"),
        exit_code: output.status.code().expect("failed to get exit code"),
    }
}

macro_rules! diff_tests {
    ( $base_dir: expr, $( ( $group: ident, ( $( $test: ident, )* ), ), )+ ) => {
        static BASE_DIR: &str = $base_dir;
        $(
        mod $group {
            $(
            #[test]
            fn $test() {
                let group = stringify!($group).trim_end_matches('_');
                let group = if group == "root" {
                  "".into()
                } else {
                  format!["{group}/"]
                };
                let test = stringify!($test).trim_end_matches('_');
                let path = format!["{}/{group}{test}.lox", super::BASE_DIR];
                super::run_diff_test(&path);
            }
            )*
        }
        )+
    };
}

mod craftinginterpreters {
    use super::*;
    diff_tests!(
        "tests/craftinginterpreters/test",
        (
            root,
            (
                empty_file, precedence,
                // 24 unexpected_character
            ),
        ),
        (
            assignment,
            (
                associativity,
                global,
                grouping,
                infix_operator,
                local,
                prefix_operator,
                syntax,
                // 26 to_this,
                undefined,
            ),
        ),
        (block, (empty, scope,),),
        (bool_, (equality, not,),),
        // 24 (call, (),),
        // 26 (class, (),),
        // 25 (closure, (),),
        (
            comments,
            (
                line_at_eof,
                only_line_comment_and_line,
                only_line_comment,
                unicode,
            ),
        ),
        // 26 (constructor, (),),
        // 26 (field, (),),
        (
            for_,
            (
                class_in_body,
                // 24 closure_in_body,
                fun_in_body,
                // 24 return_closure,
                // 24 return_inside,
                scope,
                statement_condition,
                statement_increment,
                statement_initializer,
                // 24 syntax,
                var_in_body,
            ),
        ),
        // 24 (function, (),),
        (
            if_,
            (
                class_in_else,
                class_in_then,
                dangling_else,
                else_,
                fun_in_else,
                fun_in_then,
                if_,
                truth,
                var_in_else,
                var_in_then,
            ),
        ),
        //26 (inheritance, (),),
        (
            limit,
            (
                loop_too_large,
                // 24 no_reuse_constants,
                // 24 stack_overflow,
                // 24 too_many_constants,
                // 24 too_many_locals,
                // 24 too_many_upvalues,
            ),
        ),
        (logical_operator, (and, and_truth, or, or_truth,),),
        // 26 (method, (),),
        (nil, (literal,),),
        (
            number,
            (
                // 26 decimal_point_at_eof,
                leading_dot,
                literals,
                nan_equality,
                // 26 trailing_dot,
            ),
        ),
        (
            operator,
            (
                add_bool_nil,
                add_bool_num,
                add_bool_string,
                add,
                add_nil_nil,
                add_num_nil,
                add_string_nil,
                comparison,
                divide,
                divide_nonnum_num,
                divide_num_nonnum,
                // 26 equals_class,
                equals,
                // 26 equals_method,
                greater_nonnum_num,
                greater_num_nonnum,
                greater_or_equal_nonnum_num,
                greater_or_equal_num_nonnum,
                less_nonnum_num,
                less_num_nonnum,
                less_or_equal_nonnum_num,
                less_or_equal_num_nonnum,
                multiply,
                multiply_nonnum_num,
                multiply_num_nonnum,
                negate,
                negate_nonnum,
                // 26 not_class,
                not_equals,
                // 26 not,
                subtract,
                subtract_nonnum_num,
                subtract_num_nonnum,
            ),
        ),
        (print, (missing_argument,),),
        // 24 (regression, (),),
        // 24 (return_, (),),
        (
            string,
            (error_after_multiline, literals, multiline, unterminated,),
        ),
        // 26 (super_, (),),
        // 26 (this, (),),
        (
            variable,
            (
                // 24 collide_with_parameter,
                duplicate_local,
                // 24 duplicate_parameter,
                // 25 early_bound,
                in_middle_of_block,
                in_nested_block,
                // 26 local_from_method,
                redeclare_global,
                redefine_global,
                scope_reuse_in_different_blocks,
                shadow_and_local,
                shadow_global,
                shadow_local,
                undefined_global,
                undefined_local,
                uninitialized,
                unreached_undefined,
                use_false_as_var,
                use_global_in_initializer,
                use_local_in_initializer,
                use_nil_as_var,
                use_this_as_var,
            ),
        ),
        (
            while_,
            (
                class_in_body,
                // 25 closure_in_body,
                fun_in_body,
                // 25 return_closure,
                // 25 return_inside,
                syntax,
                var_in_body,
            ),
        ),
    );
}

mod custom {
    use super::*;
    diff_tests!("tests", (print, (missing_semicolon,),),);
}
