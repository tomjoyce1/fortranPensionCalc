program RetirementCalculator
    implicit none
    real :: salary, age, retirementAge, result
    real :: salaryGrowth = 0.025
    real :: inflation = 0.025
    real :: annualReturn = 0.06

    interface
        real function pensionCalculator(salary, salaryGrowth, age, retirementAge, annualReturn, inflation)
            real :: salary, salaryGrowth, age, retirementAge, annualReturn, inflation
        end function pensionCalculator
    end interface    

    ! Placeholder values
    salary = 50000.0
    age = 20.0
    retirementAge = 67.0
 
    result = pensionCalculator(salary, salaryGrowth, age, retirementAge, annualReturn, inflation)
    print *, "THE REAL FINAL AMOUNT IS ", result
 
end program RetirementCalculator

real function pensionCalculator(salary, salaryGrowth, age, retirementAge, annualReturn, inflation)
! takes in a salary, age, retirementAge, pcSalaryContributedToPension
! prints each years salary and diff to previous year
! returns a sum of pensionSize when retirementAge reached
! assume current years pension contribution added at year end
    implicit none
    real :: salary, salaryGrowth, age, retirementAge, annualReturn, inflation
    real :: totalPensionPot, amountContributedToPension, pcSalaryContributedToPension, capitalGrowth, &
    realPensionPotValue, startingAge

    totalPensionPot = 0.0

    startingAge = age

    DO WHILE(age < retirementAge) 

        ! max pension contribution of salary
        !Under 30: 15% of gross earnings. 
        ! 30-39: 20% of gross earnings. 
        ! 40-49: 25% of gross earnings. 
        ! 50-54: 30% of gross earnings. 
        ! 55-59: 35% of gross earnings. 
        ! 60 or over: 40% of gross earnings. 

        if(age < 30) then
            pcSalaryContributedToPension = 0.15
        else if(age >= 30 .and. age <= 39) then
            pcSalaryContributedToPension = 0.20
        else if(age >= 40 .and. age <= 49) then
            pcSalaryContributedToPension = 0.25
        else if(age >= 50 .and. age <= 54) then
            pcSalaryContributedToPension = 0.30
        else if(age >= 55 .and. age <= 59) then
            pcSalaryContributedToPension = 0.35    
        else
            pcSalaryContributedToPension = 0.4    
        end if
        capitalGrowth = totalPensionPot * annualReturn

        amountContributedToPension = salary * pcSalaryContributedToPension
        
        totalPensionPot = totalPensionPot + amountContributedToPension + capitalGrowth

        realPensionPotValue = totalPensionPot / (1 + inflation)**(age - startingAge)


        print "(A, F4.1, A, F8.1, A, F3.2, A, F8.0, A, F10.1, A, F10.1, A, F10.1)", "Age: ", age, " | Salary: ", salary, & 
        " | % Contribution: ", pcSalaryContributedToPension,  " | Contributed: ",  amountContributedToPension, &
        " | Pot Growth at 6%", capitalGrowth, " |  Total Pot = ", totalPensionPot, &
        " | Inflation Adjusted Pot: ", realPensionPotValue
        age = age + 1.0

        salary = salary + (salary * salaryGrowth)

    END DO

    pensionCalculator = realPensionPotValue
end function pensionCalculator