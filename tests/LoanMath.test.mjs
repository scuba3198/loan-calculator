import test from "node:test";
import assert from "node:assert/strict";
import * as LoanMath from "../src/LoanMath.bs.js";
import * as LoanPersistence from "../src/LoanPersistence.bs.js";

const unwrapOk = (result) => {
  assert.equal(result.TAG, "Ok");
  return result._0;
};

const unwrapError = (result) => {
  assert.equal(result.TAG, "Error");
  return result._0;
};

const parseValid = (principal, annualRate, tenureMonths) =>
  unwrapOk(LoanMath.parseLoanInput(String(principal), String(annualRate), String(tenureMonths)));

const calculate = (style, input = parseValid(300000, 7.25, 12)) =>
  unwrapOk(LoanMath.calculate(input, LoanMath.repaymentStyleFromString(style)));

test("flat-rate calculation balances the schedule", () => {
  const result = calculate("flat");

  assert.equal(result.monthlyPayment, 26812.5);
  assert.equal(result.totalInterest, 21750);
  assert.equal(result.totalRepaid, 321750);
  assert.equal(result.schedule.length, 12);
  assert.equal(result.schedule.at(-1).balance, 0);
  assert.equal(result.schedule.reduce((sum, row) => sum + row.principal, 0), 300000);
});

test("EMI handles zero interest and reducing balance correctly", () => {
  const zeroInterest = calculate("emi", parseValid(120000, 0, 12));
  assert.equal(zeroInterest.monthlyPayment, 10000);
  assert.equal(zeroInterest.totalInterest, 0);
  assert.equal(zeroInterest.schedule.at(-1).balance, 0);

  const standard = calculate("emi");
  assert.ok(Math.abs(standard.monthlyPayment - 25992.611640106086) < 0.000001);
  assert.ok(standard.schedule[0].interest > standard.schedule.at(-1).interest);
  assert.equal(standard.schedule.at(-1).balance, 0);
});

test("bullet repayment charges interest monthly and principal at maturity", () => {
  const result = calculate("bullet");

  assert.equal(result.monthlyPayment, 1812.5);
  assert.equal(result.schedule[0].principal, 0);
  assert.equal(result.schedule.at(-1).principal, 300000);
  assert.equal(result.schedule.at(-1).payment, 301812.5);
  assert.equal(result.schedule.at(-1).balance, 0);
});

test("input validation rejects malformed and unsafe values", () => {
  assert.equal(unwrapError(LoanMath.parseLoanInput("", "7.25", "12")), "InvalidPrincipal");
  assert.equal(unwrapError(LoanMath.parseLoanInput("300000", "-1", "12")), "InvalidAnnualRate");
  assert.equal(unwrapError(LoanMath.parseLoanInput("300000", "7.25", "0")), "InvalidTenure");
  assert.equal(unwrapError(LoanMath.parseLoanInput("300000", "7.25", "1201")), "TenureTooLong");
  assert.equal(unwrapError(LoanMath.parseLoanInput("Infinity", "7.25", "12")), "InvalidPrincipal");
});

test("input parsing rejects trailing characters and fractional tenures", () => {
  assert.equal(
    unwrapError(LoanMath.parseLoanInput("300000oops", "7.25", "12")),
    "InvalidPrincipal",
  );
  assert.equal(
    unwrapError(LoanMath.parseLoanInput("300000", "7.25%", "12")),
    "InvalidAnnualRate",
  );
  assert.equal(
    unwrapError(LoanMath.parseLoanInput("300000", "7.25", "12.5")),
    "InvalidTenure",
  );
  assert.equal(
    unwrapOk(LoanMath.parseLoanInput("300000", "7.25", "12.0")).tenureMonths,
    12,
  );
});

test("staged disbursements keep commitment separate from funded principal", () => {
  const current = unwrapOk(
    LoanMath.parseStagedLoanInput("500000", "12", "12", [["100000", "1"]]),
  );
  const currentResult = unwrapOk(
    LoanMath.calculateStaged(current, LoanMath.repaymentStyleFromString("flat")),
  );

  assert.equal(currentResult.plannedPrincipal, 500000);
  assert.equal(currentResult.disbursedPrincipal, 100000);
  assert.equal(currentResult.totalInterest, 12000);

  const staged = unwrapOk(
    LoanMath.parseStagedLoanInput("500000", "12", "12", [
      ["100000", "1"],
      ["400000", "3"],
    ]),
  );
  const flat = unwrapOk(
    LoanMath.calculateStaged(staged, LoanMath.repaymentStyleFromString("flat")),
  );

  assert.equal(flat.totalInterest, 52000);
  assert.ok(Math.abs(flat.schedule[0].payment - 9333.333333333334) < 0.000001);
  assert.ok(Math.abs(flat.schedule[2].payment - 53333.333333333336) < 0.000001);
  assert.equal(flat.schedule.at(-1).balance, 0);

  const emi = unwrapOk(
    LoanMath.calculateStaged(staged, LoanMath.repaymentStyleFromString("emi")),
  );
  assert.ok(emi.schedule[2].payment > emi.schedule[1].payment);
  assert.equal(emi.schedule.at(-1).balance, 0);
});

test("staged disbursement validation protects the commitment and tenure", () => {
  assert.equal(
    unwrapOk(
      LoanMath.parseStagedLoanInput("0.30", "0", "1", [
        ["0.10", "1"],
        ["0.20", "1"],
      ]),
    ).plannedPrincipal,
    0.3,
  );
  assert.equal(
    unwrapError(
      LoanMath.parseStagedLoanInput("500000", "12", "12", [["500001", "1"]]),
    ),
    "DisbursementsExceedCommitment",
  );
  assert.equal(
    unwrapError(
      LoanMath.parseStagedLoanInput("500000", "12", "12", [["100000", "13"]]),
    ),
    "InvalidDisbursement",
  );
});

test("profile import rejects malformed numeric strings", () => {
  const malformed = JSON.stringify({
    profiles: [{
      name: "Malformed",
      purpose: "",
      principal: "1000oops",
      annualRate: "7.25%",
      tenureMonths: "12months",
      style: "emi",
    }],
  });

  assert.equal(unwrapError(LoanPersistence.decodeProfiles(malformed)), "InvalidLoan");
});

test("profile import rejects disbursements outside the loan plan", () => {
  const invalid = JSON.stringify({
    profiles: [{
      name: "Business Loan",
      purpose: "Working capital",
      principal: "500000",
      annualRate: "12",
      tenureMonths: "12",
      style: "flat",
      disbursements: [{amount: "100000", month: "13"}],
    }],
  });

  assert.equal(unwrapError(LoanPersistence.decodeProfiles(invalid)), "InvalidLoan");
});

test("EMI schedule remains consistent for high-rate long-term loans", () => {
  const result = calculate("emi", parseValid(100000, 36, 1200));
  const scheduleTotal = result.schedule.reduce((sum, row) => sum + row.payment, 0);

  assert.ok(Math.abs(scheduleTotal - result.totalRepaid) < 0.01);
  assert.ok(result.schedule[0].principal > 0);
  assert.ok(result.schedule.at(-1).payment < result.monthlyPayment * 1.01);
  assert.equal(result.schedule.at(-1).balance, 0);
});

test("paid months are bounded and deduplicated", () => {
  assert.deepEqual(
    LoanMath.normalizePaidMonths(12, [1, 1, 0, 13, -2, 3]),
    [1, 3],
  );
  assert.deepEqual(LoanMath.allPaidMonths(3), [1, 2, 3]);
  assert.deepEqual(LoanMath.allPaidMonths(0), []);
});

test("JSON persistence round-trips and normalizes payment history", () => {
  const json = LoanPersistence.encode(
    300000,
    7.25,
    12,
    LoanMath.repaymentStyleFromString("emi"),
    [1, 1, 0, 13],
    [2],
    [3],
  );
  const saved = unwrapOk(LoanPersistence.decode(json));

  assert.equal(saved.principal, 300000);
  assert.equal(saved.annualRate, 7.25);
  assert.equal(saved.tenureMonths, 12);
  assert.equal(saved.style, "Emi");
  assert.deepEqual(saved.flatPaidMonths, [1]);
  assert.deepEqual(saved.emiPaidMonths, [2]);
  assert.deepEqual(saved.bulletPaidMonths, [3]);
});

test("JSON persistence supports legacy histories and rejects invalid files", () => {
  const legacy = LoanPersistence.decode(
    '{"principal":1000,"annualRate":0,"tenureMonths":3,"style":"bullet","paidMonths":[1,3,99]}',
  );
  const saved = unwrapOk(legacy);
  assert.deepEqual(saved.flatPaidMonths, []);
  assert.deepEqual(saved.emiPaidMonths, []);
  assert.deepEqual(saved.bulletPaidMonths, [1, 3]);

  assert.equal(unwrapError(LoanPersistence.decode("not json")), "InvalidJson");
  assert.equal(
    unwrapError(LoanPersistence.decode('{"principal":1000,"annualRate":0,"tenureMonths":3,"style":"unknown"}')),
    "InvalidFormat",
  );
  assert.equal(
    unwrapError(LoanPersistence.decode('{"principal":1000,"annualRate":0,"tenureMonths":3,"paidMonths":{}}')),
    "InvalidFormat",
  );
});

test("profile persistence round-trips multiple named loans", () => {
  const purchase = {
    ...LoanPersistence.createProfile("Laptop Purchase", "Personal purchase"),
    principalInput: "90000",
    annualRateInput: "12",
    tenureMonthsInput: "12",
    style: "Emi",
    emiPaidMonths: [1, 2],
    disbursements: [{amountInput: "90000", monthInput: "1"}],
  };
  const friendLoan = {
    ...LoanPersistence.createProfile("Friend Loan", "Business working capital"),
    principalInput: "250000",
    annualRateInput: "8",
    tenureMonthsInput: "18",
    style: "FlatRate",
    flatPaidMonths: [1],
    disbursements: [
      {amountInput: "100000", monthInput: "1"},
      {amountInput: "150000", monthInput: "3"},
    ],
  };

  const json = LoanPersistence.encodeProfiles([purchase, friendLoan], 1);
  const saved = unwrapOk(LoanPersistence.decodeProfiles(json));

  assert.equal(saved.activeProfileIndex, 1);
  assert.equal(saved.profiles.length, 2);
  assert.equal(saved.profiles[0].name, "Laptop Purchase");
  assert.equal(saved.profiles[0].purpose, "Personal purchase");
  assert.equal(saved.profiles[0].style, "Emi");
  assert.deepEqual(saved.profiles[0].emiPaidMonths, [1, 2]);
  assert.equal(saved.profiles[1].name, "Friend Loan");
  assert.equal(saved.profiles[1].style, "FlatRate");
  assert.deepEqual(saved.profiles[1].flatPaidMonths, [1]);
  assert.deepEqual(saved.profiles[1].disbursements, [
    {amountInput: "100000", monthInput: "1"},
    {amountInput: "150000", monthInput: "3"},
  ]);
});

test("profile persistence wraps the legacy single-loan format", () => {
  const saved = unwrapOk(LoanPersistence.decodeProfiles(
    '{"principal":1000,"annualRate":0,"tenureMonths":3,"style":"bullet","paidMonths":[1,3]}',
  ));

  assert.equal(saved.profiles.length, 1);
  assert.equal(saved.profiles[0].name, "Imported Loan");
  assert.equal(saved.profiles[0].style, "Bullet");
  assert.deepEqual(saved.profiles[0].bulletPaidMonths, [1, 3]);
  assert.deepEqual(saved.profiles[0].disbursements, [{amountInput: "1000", monthInput: "1"}]);
});
