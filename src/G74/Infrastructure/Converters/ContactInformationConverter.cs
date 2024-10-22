using G74.Domain.Value_Objects.Patient;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace G74.Infrastructure.Converters;

public class ContactInformationConverter : ValueConverter<ContactInformation, string>
{
 
    public ContactInformationConverter() 
        : base(
            v => v.ToString(), // Convert ContactInfo to string
            v => ContactInformation.FromString(v) // Convert string back to ContactInfo
        )
    {
    }
    
    
}

