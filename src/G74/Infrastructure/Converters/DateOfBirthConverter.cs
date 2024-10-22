using G74.Domain.Value_Objects.SharedValueObjects;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace G74.Infrastructure.Converters;

public class DateOfBirthConverter : ValueConverter<DateOfBirth, string>
{
    public DateOfBirthConverter()
        : base(
            v => v.ShowFormattedDateOfBirth(), // Convert DateOfBirth to string
            v => DateOfBirth.FromString(v) // Convert string back to DateOfBirth
        )
    {
    }
}