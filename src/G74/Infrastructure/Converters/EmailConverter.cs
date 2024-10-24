using G74.Domain.Value_Objects;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace G74.Infrastructure.Converters;

public class EmailConverter : ValueConverter<Email, string>
{
    public EmailConverter()
        : base(
            u => u.email, 
            value => new Email(value) 
        )
    {
    }
}