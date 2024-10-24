using G74.Domain.Value_Objects;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace G74.Infrastructure.Converters;

public class UsernameConverter : ValueConverter<Username, string>
{
    public UsernameConverter()
        : base(
            u => u.name, 
            value => new Username(value) 
        )
    {
    }
}