using G74.Domain.Value_Objects.SharedValueObjects;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace G74.Infrastructure.Converters;

public class NameConverter : ValueConverter<Name, string>
{
    public NameConverter()
        : base(
            v => v.ToString(),
            v => new Name(v)
        )
    {
        
    }
}