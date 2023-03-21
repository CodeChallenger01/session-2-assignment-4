import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class BankApplicationTest extends AnyFlatSpec with Matchers {
  val accounts: Map[Long, Double] = Map()
  val bankApp = new Bank
  val accountDetails = accounts ++ bankApp.createAccount(15000) ++ bankApp.createAccount(14000)
  val listOfAccountNumber = accountDetails.keys.toList
  val listOfAccounts = accountDetails.values.toList

  "Empty Account" should "match when we check with empty account" in {
    val isAccountEmpty = accounts.isEmpty
    assert(isAccountEmpty == true)
  }

  "check amount of first account" should "return the amount of first account " in {
    val openingBalance = 12000.0
    val account = bankApp.createAccount(openingBalance)
    val accountAmount = account.values.toList
    accountAmount(0) shouldBe openingBalance
  }

  "check size" should " match with exist account size and including inside account as well" in {
    val newAccount = accountDetails ++ bankApp.createAccount(20000)
    val noOfAccounts = newAccount.size
    assert(noOfAccounts == newAccount.size)
  }

  "listAccount" should "return the list of accounts" in {
    val listOfAccounts = bankApp.listAccounts(accountDetails)
    val actualOutput = accountDetails
    listOfAccounts shouldBe actualOutput
  }

  "it " should "contains all the account" in {
    val listOfAccountNumber = accountDetails.keys.toList
    val listOfAmounts = accountDetails.values.toList
    bankApp.listAccounts(accountDetails) shouldBe Map(listOfAccountNumber(0) -> listOfAmounts(0), listOfAccountNumber(1) -> listOfAmounts(1))
  }

  "listAccount" should "not return empty map because it contains the account" in {
    val listOfAccount = bankApp.listAccounts(accountDetails)
    assert(listOfAccount != null)
  }

  "fetch Account Balance" should " return the bank balance" in {
    val accountNumber = accountDetails.keys.toList
    val fetchBalance = bankApp.fetchAccountBalance(accountNumber(0), accountDetails)
    val expectedBalanceOfFirstAccount = accountDetails.values.toList(0)
    fetchBalance shouldBe expectedBalanceOfFirstAccount
  }

  "fetch Account Balance" should " return the bank balance not equal to if we are checking with other" in {
    val accountNumber = accountDetails.keys.toList
    val fetchBalance = bankApp.fetchAccountBalance(accountNumber(0), accountDetails)
    val expectedBalanceOfFirstAccount = accountDetails.values.toList(1)
    assert(fetchBalance != expectedBalanceOfFirstAccount)
  }

  "delete Account" should "return true if account found and deleted" in {
    val accountNumber = accountDetails.keys.toList
    val isAccountDeleted = bankApp.deleteAccount(accountNumber(0), accountDetails)
    val expectedOutput = true
    isAccountDeleted shouldBe expectedOutput
  }

  "delete Account" should "return false if account is not found" in {
    val accountNumber = 200202022002L
    val isAccountDeleted = bankApp.deleteAccount(accountNumber, accountDetails)
    val expectedOutput = true
    assert(isAccountDeleted != true)
  }

  "update Balance" should " match with updated balance" in {
    val accountNumberList = accountDetails.keys.toList
    val transactions = List(
      Transactions(3l, accountNumberList(0), "Credit", 12000.0)
    )
    val balance = bankApp.updateBalance(transactions, accountDetails).values.toList(0)
    val expectedBalance = accountDetails.values.toList(0) + 12000.0
    balance shouldBe expectedBalance
  }

  "updated Balance" should "not match with balance if account number not exist" in {
    val accountNumber = 200202022002L
    val transactions = List(
      Transactions(3l, accountNumber, "Credit", 12000.0)
    )
    val balance = bankApp.updateBalance(transactions, accountDetails).values.toList(0)
    println(balance)
    val expectedBalance = 0
    assert(balance != expectedBalance)
  }


}
