using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace G74.Infrastructure.Converters;

public class GenderConverter : ValueConverter<Gender, string>
{

    public GenderConverter()
        : base(
            v => v.ToString(),
            v => Gender.FromString(v)
        )
    {
    }
}