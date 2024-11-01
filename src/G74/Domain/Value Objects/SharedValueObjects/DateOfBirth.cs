using G74.Domain.Shared;


namespace G74.Domain.Value_Objects.SharedValueObjects;

public class DateOfBirth : IValueObject, IEquatable<DateOfBirth>
{
    public DateTime dateOfBirth { get; }

    private const string InvalidDateMsg = "Invalid date provided";
    private const string DateInFutureMsg = "Date of birth cannot be in the future";

    public DateOfBirth(int yearOfBirth, int monthOfBirth, int dayOfBirth)
    {
        dateOfBirth = BirthDateValidations(yearOfBirth, monthOfBirth, dayOfBirth);
    }

    public DateOfBirth(DateOfBirth other)
    {
        BirthDateValidations(other.dateOfBirth.Year, other.dateOfBirth.Month, other.dateOfBirth.Day);

        dateOfBirth = other.dateOfBirth;
    }

    public static DateTime BirthDateValidations(int yearOfBirth, int monthOfBirth, int dayOfBirth)
    {
        DateTime validateDateTime;

        try
        {
            validateDateTime = new DateTime(yearOfBirth, monthOfBirth, dayOfBirth);
        }
        catch (ArgumentOutOfRangeException ex)
        {
            throw new BusinessRuleValidationException(InvalidDateMsg, ex.Message);
        }

        if (validateDateTime > DateTime.UtcNow)
        {
            throw new BusinessRuleValidationException(DateInFutureMsg);
        }

        return validateDateTime;
    }

    public static DateOfBirth FromString(string dateOfBirthString)
    {
        var parts = dateOfBirthString.Split(new[] { ',', '/' });

        if (parts.Length != 3
            || !int.TryParse(parts[0], out int year)
            || !int.TryParse(parts[1], out int month)
            || !int.TryParse(parts[2], out int day))
        {
            throw new BusinessRuleValidationException(
                "Date of birth must be in the format 'YYYY-MM-DD' or 'YYYY/MM/DD'.");
        }

        return new DateOfBirth(year, month, day);
    }

    public string ToFormattedDateOfBirthStr()
    {
        return $"{dateOfBirth.Year:D4}-{dateOfBirth.Month:D2}-{dateOfBirth.Day:D2}";
    }

    public bool Equals(DateOfBirth? other) => other != null && dateOfBirth == other.dateOfBirth;

    public override bool Equals(object? obj) => obj is DateOfBirth other && Equals(other);

    public override int GetHashCode() => dateOfBirth.GetHashCode();
}