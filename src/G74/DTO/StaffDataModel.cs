using G74.Domain.Aggregates.Staff;
using G74.Domain.Shared;

namespace G74.DTO;

public class StaffDataModel : Entity<Guid>
{
    public string LicenseNumber { get; set; }
    public string Name { get; set; }
    public string PhoneNumber { get; set; }
    public string ContactEmail { get; set; }
    public string StaffSpecialization { get; set; }
    public string Status { get; set; }

    
    protected StaffDataModel() : base(Guid.NewGuid())
    {
        
    }

    public StaffDataModel(Staff staff) : base(Guid.NewGuid())
    {
        // Id = staff.Id;
        LicenseNumber = staff.LicenseNumber.Value;
        Name = staff.Name.TheName;
        PhoneNumber = staff.PhoneNumber.Value;
        ContactEmail = staff.ContactEmail.email;
        StaffSpecialization = staff.StaffSpecialization.Value;
        Status = staff.Status.Value;
    }
    
    
}