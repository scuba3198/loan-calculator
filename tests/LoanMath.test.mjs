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
  };
  const friendLoan = {
    ...LoanPersistence.createProfile("Friend Loan", "Business working capital"),
    principalInput: "250000",
    annualRateInput: "8",
    tenureMonthsInput: "18",
    style: "FlatRate",
    flatPaidMonths: [1],
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
});

test("profile persistence wraps the legacy single-loan format", () => {
  const saved = unwrapOk(LoanPersistence.decodeProfiles(
    '{"principal":1000,"annualRate":0,"tenureMonths":3,"style":"bullet","paidMonths":[1,3]}',
  ));

  assert.equal(saved.profiles.length, 1);
  assert.equal(saved.profiles[0].name, "Imported Loan");
  assert.equal(saved.profiles[0].style, "Bullet");
  assert.deepEqual(saved.profiles[0].bulletPaidMonths, [1, 3]);
});
