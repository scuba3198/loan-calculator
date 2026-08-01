type savedState = {
  principal: float,
  annualRate: float,
  tenureMonths: int,
  style: LoanMath.repaymentStyle,
  flatPaidMonths: array<int>,
  emiPaidMonths: array<int>,
  bulletPaidMonths: array<int>,
}

type disbursementInput = {
  amountInput: string,
  monthInput: string,
}

type profile = {
  name: string,
  purpose: string,
  principalInput: string,
  annualRateInput: string,
  tenureMonthsInput: string,
  style: LoanMath.repaymentStyle,
  flatPaidMonths: array<int>,
  emiPaidMonths: array<int>,
  bulletPaidMonths: array<int>,
  disbursements: array<disbursementInput>,
}

type savedProfiles = {
  profiles: array<profile>,
  activeProfileIndex: int,
}

type importError =
  | InvalidJson
  | InvalidFormat
  | InvalidLoan

@module("./LoanFile.js")
external downloadJson: (string, string) => unit = "downloadJson"

@module("./LoanFile.js")
external readFileAsText: ('a, string => unit, string => unit) => unit = "readFileAsText"

@module("./LoanFile.js")
external clearFileInput: 'a => unit = "clearFileInput"

@module("./LoanFile.js")
external clickFileInput: 'a => unit = "clickFileInput"

let createProfile = (~name: string, ~purpose: string): profile => {
  let defaultPrincipal = Belt.Float.toString(LoanMath.defaultInput.principal)
  {
    name,
    purpose,
    principalInput: defaultPrincipal,
    annualRateInput: Belt.Float.toString(LoanMath.defaultInput.annualRate),
    tenureMonthsInput: Belt.Int.toString(LoanMath.defaultInput.tenureMonths),
    style: LoanMath.FlatRate,
    flatPaidMonths: [],
    emiPaidMonths: [],
    bulletPaidMonths: [],
    disbursements: [{amountInput: defaultPrincipal, monthInput: "1"}],
  }
}

let defaultProfile = createProfile(~name="My Loan", ~purpose="")

let jsonField = (object: dict<JSON.t>, key: string): option<JSON.t> => Dict.get(object, key)

let decodeFloat = (object: dict<JSON.t>, key: string): option<float> =>
  switch jsonField(object, key) {
  | Some(value) => JSON.Decode.float(value)
  | None => None
  }

let decodeInt = (value: JSON.t): option<int> =>
  switch JSON.Decode.float(value) {
  | Some(number) =>
    if Float.isFinite(number) && number >= -2147483648.0 && number <= 2147483647.0 {
      let integer = Belt.Float.toInt(number)
      if Belt.Int.toFloat(integer) == number {
        Some(integer)
      } else {
        None
      }
    } else {
      None
    }
  | None => None
  }

let decodeIntArray = (value: JSON.t): option<array<int>> =>
  switch JSON.Decode.array(value) {
  | Some(values) =>
    Some(values->Belt.Array.reduce([], (decoded, value) => {
      if Belt.Array.length(decoded) >= LoanMath.maxTenureMonths {
        decoded
      } else {
        switch decodeInt(value) {
        | Some(number) => Belt.Array.concat(decoded, [number])
        | None => decoded
        }
      }
    }))
  | None => None
  }

let decodeOptionalIntArray = (object: dict<JSON.t>, key: string): option<array<int>> =>
  switch jsonField(object, key) {
  | None => Some([])
  | Some(value) => decodeIntArray(value)
  }

let decodeInputString = (object: dict<JSON.t>, key: string): option<string> =>
  switch jsonField(object, key) {
  | None => None
  | Some(value) =>
    switch JSON.Decode.string(value) {
    | Some(text) => Some(text)
    | None =>
      switch JSON.Decode.float(value) {
      | Some(number) if Float.isFinite(number) => Some(Belt.Float.toString(number))
      | _ => None
      }
    }
  }

let decodeDisbursementArray = (value: JSON.t): option<array<disbursementInput>> =>
  switch JSON.Decode.array(value) {
  | None => None
  | Some(values) => values->Belt.Array.reduce(Some([]), (decoded, value) => switch decoded {
    | None => None
    | Some(disbursements) => switch JSON.Decode.object(value) {
      | None => None
      | Some(object) => switch (decodeInputString(object, "amount"), decodeInputString(object, "month")) {
        | (Some(amountInput), Some(monthInput)) => Some(Belt.Array.concat(disbursements, [{amountInput, monthInput}]))
        | _ => None
        }
      }
    })
  }

let decodeDisbursements = (
  object: dict<JSON.t>,
  fallbackPrincipal: string,
): option<array<disbursementInput>> => switch jsonField(object, "disbursements") {
| None => Some([{amountInput: fallbackPrincipal, monthInput: "1"}])
| Some(value) => decodeDisbursementArray(value)
}

let decodeLabel = (object: dict<JSON.t>, key: string, fallback: string): result<string, importError> =>
  switch jsonField(object, key) {
  | None => Ok(fallback)
  | Some(value) =>
    switch JSON.Decode.string(value) {
    | Some(text) => Ok(text)
    | None => Error(InvalidFormat)
    }
  }

let decodeStyle = (object: dict<JSON.t>): option<LoanMath.repaymentStyle> =>
  switch jsonField(object, "style") {
  | None => Some(LoanMath.FlatRate)
  | Some(value) => switch JSON.Decode.string(value) {
    | Some(style) => LoanMath.repaymentStyleFromString(style)
    | None => None
    }
  }

let decodePaidMonths = (
  ~root: dict<JSON.t>,
  ~style: LoanMath.repaymentStyle,
  ~tenureMonths: int,
): result<(array<int>, array<int>, array<int>), importError> => {
  switch jsonField(root, "paidMonthsByStyle") {
  | Some(value) => switch JSON.Decode.object(value) {
    | Some(byStyle) =>
      switch (
        decodeOptionalIntArray(byStyle, "flat"),
        decodeOptionalIntArray(byStyle, "emi"),
        decodeOptionalIntArray(byStyle, "bullet"),
      ) {
      | (Some(flat), Some(emi), Some(bullet)) =>
        Ok((
          LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=flat),
          LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=emi),
          LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=bullet),
        ))
      | _ => Error(InvalidFormat)
      }
    | None => Error(InvalidFormat)
    }
  | None =>
    switch switch jsonField(root, "paidMonths") {
    | Some(value) => decodeIntArray(value)
    | None => Some([])
    } {
    | None => Error(InvalidFormat)
    | Some(legacyMonths) =>
      let normalized = LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=legacyMonths)
      switch style {
      | LoanMath.FlatRate => Ok((normalized, [], []))
      | LoanMath.Emi => Ok(([], normalized, []))
      | LoanMath.Bullet => Ok(([], [], normalized))
      }
    }
  }
}

let decode = (content: string): result<savedState, importError> => {
  try {
    let root = JSON.parseOrThrow(content)
    switch JSON.Decode.object(root) {
    | None => Error(InvalidFormat)
    | Some(root) =>
      switch (
        decodeFloat(root, "principal"),
        decodeFloat(root, "annualRate"),
        jsonField(root, "tenureMonths"),
        decodeStyle(root),
      ) {
      | (Some(principal), Some(annualRate), Some(tenureValue), Some(style)) =>
        switch decodeInt(tenureValue) {
        | None => Error(InvalidLoan)
        | Some(tenureMonths) =>
          switch LoanMath.validateLoanInput({principal, annualRate, tenureMonths}) {
          | Error(_) => Error(InvalidLoan)
          | Ok(_) => switch decodePaidMonths(~root, ~style, ~tenureMonths) {
            | Error(error) => Error(error)
            | Ok((flatPaidMonths, emiPaidMonths, bulletPaidMonths)) =>
              Ok({
                principal,
                annualRate,
                tenureMonths,
                style,
                flatPaidMonths,
                emiPaidMonths,
                bulletPaidMonths,
              })
            }
          }
        }
      | _ => Error(InvalidFormat)
      }
    }
  } catch {
  | JsExn(_) => Error(InvalidJson)
  }
}

let profileFromSavedState = (saved: savedState): profile => {
  let principalInput = Belt.Float.toString(saved.principal)
  {
    name: "Imported Loan",
    purpose: "",
    principalInput,
    annualRateInput: Belt.Float.toString(saved.annualRate),
    tenureMonthsInput: Belt.Int.toString(saved.tenureMonths),
    style: saved.style,
    flatPaidMonths: saved.flatPaidMonths,
    emiPaidMonths: saved.emiPaidMonths,
    bulletPaidMonths: saved.bulletPaidMonths,
    disbursements: [{amountInput: principalInput, monthInput: "1"}],
  }
}

let decodeProfile = (object: dict<JSON.t>): result<profile, importError> =>
  switch (decodeLabel(object, "name", "Untitled Loan"), decodeLabel(object, "purpose", "")) {
  | (Error(error), _) => Error(error)
  | (_, Error(error)) => Error(error)
  | (Ok(name), Ok(purpose)) =>
    switch (
      decodeInputString(object, "principal"),
      decodeInputString(object, "annualRate"),
      decodeInputString(object, "tenureMonths"),
      decodeStyle(object),
    ) {
    | (Some(principalInput), Some(annualRateInput), Some(tenureMonthsInput), Some(style)) =>
      switch decodeDisbursements(object, principalInput) {
      | None => Error(InvalidFormat)
      | Some(disbursements) =>
        switch LoanMath.parseStagedLoanInput(
          ~plannedPrincipalInput=principalInput,
          ~annualRateInput,
          ~tenureMonthsInput,
          ~disbursementInputs=disbursements->Belt.Array.map(disbursement =>
            (disbursement.amountInput, disbursement.monthInput)
          ),
        ) {
        | Error(_) => Error(InvalidLoan)
        | Ok(input) =>
          switch decodePaidMonths(~root=object, ~style, ~tenureMonths=input.tenureMonths) {
          | Error(error) => Error(error)
          | Ok((flatPaidMonths, emiPaidMonths, bulletPaidMonths)) =>
            Ok({
              name,
              purpose,
              principalInput,
              annualRateInput,
              tenureMonthsInput,
              style,
              flatPaidMonths,
              emiPaidMonths,
              bulletPaidMonths,
              disbursements,
            })
          }
        }
      }
    | _ => Error(InvalidFormat)
    }
  }

let decodeProfileArray = (value: JSON.t): result<array<profile>, importError> =>
  switch JSON.Decode.array(value) {
  | None => Error(InvalidFormat)
  | Some(values) =>
    values->Belt.Array.reduce(Ok([]), (decoded, value) => switch decoded {
    | Error(error) => Error(error)
    | Ok(profiles) =>
      switch JSON.Decode.object(value) {
      | None => Error(InvalidFormat)
      | Some(object) => switch decodeProfile(object) {
        | Error(error) => Error(error)
        | Ok(profile) => Ok(Belt.Array.concat(profiles, [profile]))
        }
      }
    })
  }

let normalizedActiveProfileIndex = (~index: int, ~profileCount: int): int => {
  if profileCount <= 0 {
    0
  } else if index < 0 {
    0
  } else if index >= profileCount {
    profileCount - 1
  } else {
    index
  }
}

let decodeProfiles = (content: string): result<savedProfiles, importError> => {
  try {
    let root = JSON.parseOrThrow(content)
    switch JSON.Decode.object(root) {
    | None => Error(InvalidFormat)
    | Some(root) => switch jsonField(root, "profiles") {
      | None => switch decode(content) {
        | Error(error) => Error(error)
        | Ok(saved) => Ok({profiles: [profileFromSavedState(saved)], activeProfileIndex: 0})
        }
      | Some(value) =>
        switch decodeProfileArray(value) {
        | Error(error) => Error(error)
        | Ok(profiles) =>
          if Belt.Array.length(profiles) == 0 {
            Error(InvalidFormat)
          } else {
            let requestedIndex = switch jsonField(root, "activeProfileIndex") {
            | Some(value) => switch decodeInt(value) {
              | Some(index) => index
              | None => 0
              }
            | None => 0
            }
            Ok({
              profiles,
              activeProfileIndex: normalizedActiveProfileIndex(
                ~index=requestedIndex,
                ~profileCount=Belt.Array.length(profiles),
              ),
            })
          }
        }
      }
    }
  } catch {
  | JsExn(_) => Error(InvalidJson)
  }
}

let encode = (
  ~principal: float,
  ~annualRate: float,
  ~tenureMonths: int,
  ~style: LoanMath.repaymentStyle,
  ~flatPaidMonths: array<int>,
  ~emiPaidMonths: array<int>,
  ~bulletPaidMonths: array<int>,
): string => {
  let normalizedFlat = LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=flatPaidMonths)
  let normalizedEmi = LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=emiPaidMonths)
  let normalizedBullet = LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=bulletPaidMonths)
  let paidMonthsByStyle = Dict.fromArray([
    ("flat", JSON.Encode.intArray(normalizedFlat)),
    ("emi", JSON.Encode.intArray(normalizedEmi)),
    ("bullet", JSON.Encode.intArray(normalizedBullet)),
  ])->JSON.Encode.object
  Dict.fromArray([
    ("version", JSON.Encode.int(2)),
    ("principal", JSON.Encode.float(principal)),
    ("annualRate", JSON.Encode.float(annualRate)),
    ("tenureMonths", JSON.Encode.int(tenureMonths)),
    ("style", JSON.Encode.string(LoanMath.repaymentStyleToString(style))),
    ("paidMonthsByStyle", paidMonthsByStyle),
  ])->JSON.Encode.object->JSON.stringify
}

let encodeProfile = (profile: profile): JSON.t => {
  let normalizedFlat = LoanMath.normalizePaidMonths(
    ~months=switch LoanMath.parseLoanInput(
      ~principalInput=profile.principalInput,
      ~annualRateInput=profile.annualRateInput,
      ~tenureMonthsInput=profile.tenureMonthsInput,
    ) {
    | Ok(input) => input.tenureMonths
    | Error(_) => 0
    },
    ~paidMonths=profile.flatPaidMonths,
  )
  let normalizedEmi = LoanMath.normalizePaidMonths(
    ~months=switch LoanMath.parseLoanInput(
      ~principalInput=profile.principalInput,
      ~annualRateInput=profile.annualRateInput,
      ~tenureMonthsInput=profile.tenureMonthsInput,
    ) {
    | Ok(input) => input.tenureMonths
    | Error(_) => 0
    },
    ~paidMonths=profile.emiPaidMonths,
  )
  let normalizedBullet = LoanMath.normalizePaidMonths(
    ~months=switch LoanMath.parseLoanInput(
      ~principalInput=profile.principalInput,
      ~annualRateInput=profile.annualRateInput,
      ~tenureMonthsInput=profile.tenureMonthsInput,
    ) {
    | Ok(input) => input.tenureMonths
    | Error(_) => 0
    },
    ~paidMonths=profile.bulletPaidMonths,
  )
  let paidMonthsByStyle = Dict.fromArray([
    ("flat", JSON.Encode.intArray(normalizedFlat)),
    ("emi", JSON.Encode.intArray(normalizedEmi)),
    ("bullet", JSON.Encode.intArray(normalizedBullet)),
  ])->JSON.Encode.object
  let disbursements = profile.disbursements->Belt.Array.map(disbursement =>
    Dict.fromArray([
      ("amount", JSON.Encode.string(disbursement.amountInput)),
      ("month", JSON.Encode.string(disbursement.monthInput)),
    ])->JSON.Encode.object
  )
  Dict.fromArray([
    ("name", JSON.Encode.string(profile.name)),
    ("purpose", JSON.Encode.string(profile.purpose)),
    ("principal", JSON.Encode.string(profile.principalInput)),
    ("annualRate", JSON.Encode.string(profile.annualRateInput)),
    ("tenureMonths", JSON.Encode.string(profile.tenureMonthsInput)),
    ("style", JSON.Encode.string(LoanMath.repaymentStyleToString(profile.style))),
    ("paidMonthsByStyle", paidMonthsByStyle),
    ("disbursements", JSON.Encode.array(disbursements)),
  ])->JSON.Encode.object
}

let encodeProfiles = (~profiles: array<profile>, ~activeProfileIndex: int): string => {
  let normalizedIndex = normalizedActiveProfileIndex(
    ~index=activeProfileIndex,
    ~profileCount=Belt.Array.length(profiles),
  )
  Dict.fromArray([
    ("version", JSON.Encode.int(4)),
    ("activeProfileIndex", JSON.Encode.int(normalizedIndex)),
    ("profiles", profiles->Belt.Array.map(encodeProfile)->JSON.Encode.array),
  ])->JSON.Encode.object->JSON.stringify
}

let importErrorMessage = (error: importError): string => switch error {
| InvalidJson => "The selected file is not valid JSON."
| InvalidFormat => "The selected file does not match the loan calculator format."
| InvalidLoan => "The imported loan values are outside the supported range."
}

let download = (content: string): unit => downloadJson("loan-calculator.json", content)

let downloadProfiles = (content: string): unit => downloadJson("loan-calculator-profiles.json", content)
