@tag("success")
type commandResult =
  | @as(true) Success({reply: string, reason: Null.t<string>})
  | @as(false) Failure({reply: string, reason: string})

type command

@send @variadic
external execute: (command, ~command: string, array<string>) => promise<commandResult> = "execute"

@val external command: command = "command"

type waitTime =
  | Seconds
  | Minutes
  | Hours
  | Days
  | Unknown

let getWaitTime = (str: string): result<(waitTime, int), string> => {
  let rec parse = (str: string, counter: int): result<(string, int), string> => {
    switch str->String.get(counter) {
    | Some(c) =>
      switch c->Int.fromString {
      // 123d
      // ^
      // counter = 0
      | Some(_) => parse(str, counter + 1)
      | None =>
        if counter > 0 {
          // 123d
          //    ^
          // counter = 2
          Ok((c, counter))
        } else {
          Error("Argument doesn't start with a number.")
        }
      }
    | None =>
      if counter > 0 {
        Ok("m", counter)
      } else {
        Error("Couldn't parse duration.")
      }
    }
  }

  let? Ok((unitStr, count)) = parse(str, 0)

  let units = switch unitStr {
  | "m" => Minutes
  | "s" => Seconds
  | "h" => Hours
  | "d" => Days
  | _ => Unknown
  }

  if units == Unknown {
    Error(
      "Invalid duration. Only minutes, seconds, hours and days supported (singular letter only).",
    )
  } else {
    let slice = str->String.slice(~start=0, ~end=count + 1)

    let? Ok(num) = switch Int.fromString(slice) {
    | Some(i) => Ok(i)
    | None => Error("Couldn't parse argument :( this probably shouldn't happen")
    }

    Ok((units, num))
  }
}

let executeReminder = async (
  timeType: waitTime,
  duration: int,
  label: option<array<string>>,
): result<string, string> => {
  let? Ok(timeString) = switch timeType {
  | Seconds => Ok("seconds")
  | Minutes => Ok("minutes")
  | Hours => Ok("hours")
  | Days => Ok("days")
  | Unknown => Error("Provided Unknown as time type")
  }

  let formatted = ["in", duration->Int.toString, timeString]

  let formatted = switch label {
  | Some(text) => Array.concat(formatted, text)
  | None => formatted
  }

  let reminder = await command->execute(~command="remindme", formatted)

  switch reminder {
  | Success({reply}) => Ok(reply)
  | Failure({reply}) => Error(reply)
  }
}

let main = async (args: array<string>): string => {
  switch args[0] {
  | None => "Please provide arguments to this alias. Can be used like \"$$r 15\" (reminds you in 15 minutes if seconds not specified)"
  | Some(arg) => {
      let waitTime = getWaitTime(arg)

      let label: option<array<string>> = if args->Array.length == 1 {
        None
      } else {
        Some(args->Array.slice(~start=1))
      }

      switch waitTime {
      | Error(reason) => reason
      | Ok((timeType, duration)) => {
          let reminder = await executeReminder(timeType, duration, label)

          switch reminder {
          | Error(err) => `Couldn't execute a reminder. Supibot error: ${err}`
          | Ok(text) => text
          }
        }
      }
    }
  }
}
