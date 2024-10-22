using G74.Domain.Value_Objects.Patient;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace G74.Infrastructure.Converters;

public class EmergencyContactConverter : ValueConverter<EmergencyContact,string>
{

    public EmergencyContactConverter()
        : base(
            contact => contact.ToString(),
            str => EmergencyContact.FromString(str)
        )
    {
        
    }




}