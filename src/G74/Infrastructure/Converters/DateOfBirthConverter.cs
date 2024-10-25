using G74.Domain.Value_Objects.SharedValueObjects;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace G74.Infrastructure.Converters;

public class DateOfBirthConverter : ValueConverter<DateOfBirth, string>
{
    public DateOfBirthConverter()
        : base(
            v => v.ToFormattedDateOfBirthStr(), 
            v => DateOfBirth.FromString(v) 
        )
    {
    }
}