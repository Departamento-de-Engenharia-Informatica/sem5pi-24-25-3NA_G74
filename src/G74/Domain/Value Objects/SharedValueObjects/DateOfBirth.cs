using G74.Domain.Shared;



namespace G74.Domain.Value_Objects.SharedValueObjects;
public class DateOfBirth : IValueObject
{
    private int YearOfBirth { get; }

    private int MonthOfBirth { get; }

    private int DayOfBirth { get; }


    public DateOfBirth(int yearOfBirth, int monthOfBirth, int dayOfBirth)
    {
        BirthDateValidations(yearOfBirth, monthOfBirth, dayOfBirth);

        YearOfBirth = yearOfBirth;
        MonthOfBirth = monthOfBirth;
        DayOfBirth = dayOfBirth;
    }

    private void BirthDateValidations(int yearOfBirth, int monthOfBirth, int dayOfBirth)
    {
        // DateTime will do the date validations
        DateTime dateOfBirth;
        try
        {
            dateOfBirth = new DateTime(yearOfBirth, monthOfBirth, dayOfBirth);
        }
        catch (ArgumentOutOfRangeException)
        {
            throw new ArgumentException(InvalidDateMsg);
        }

        // To ensure that the date of birth is not in the future
        if (dateOfBirth > DateTime.Today)
        {
            throw new ArgumentException(DateInFutureMsg);
        }
    }

    public string ShowFormattedDateOfBirth()
    {
        return $"{YearOfBirth:D4}-{MonthOfBirth:D2}-{DayOfBirth:D2}";
    }


    private const string InvalidDateMsg = "Invalid date provided";
    private const string DateInFutureMsg = "Date of birth cannot be in the future";
}