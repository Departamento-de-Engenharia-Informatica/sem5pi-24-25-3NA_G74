using G74.Domain.Value_Objects.Patient;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace G74.Infrastructure.Converters;

public class MedicalRecordNumberConverter : ValueConverter<MedicalRecordNumber, string>
{
    
    public MedicalRecordNumberConverter()
        : base(
            m => m.MedicalNumber, 
            value => new MedicalRecordNumber(value) 
        )
    {
    }
    
}