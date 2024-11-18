using G74.Domain.Aggregates.Staff;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Value_Objects.Staff.Doctor;
using G74.Domain.Value_Objects.User;

namespace G74.DTO;
public class StaffDataModel : Entity<Guid>
{
    public long LicenceNumber { get; set; }
    public string Name { get; set; }
    public string PhoneNumber { get; set; }
    public string ContactEmail { get; set; }
    public string StaffSpecialization { get; set; }
    public string Status { get; set; }
    public string Availability { get; set; }
    
    protected StaffDataModel() : base(Guid.NewGuid())
    {
        
    }
    public StaffDataModel(Staff staff) : base(Guid.NewGuid())
    {
        // Id = staff.Id;
        LicenceNumber = staff.LicenceNumber.Value;
        Name = staff.Name.Value;
        PhoneNumber = staff.PhoneNumber.Value;
        ContactEmail = staff.ContactEmail.email;
        StaffSpecialization = staff.StaffSpecialization.Value;
        Status = staff.Status.Value;
        Availability = staff.Availability;
    }
    
    // Constructor for creating from domain model (e.g., for API response)
    public static StaffDataModel FromDomain(Staff staff)
    {
        return new StaffDataModel
        {
            LicenceNumber = staff.LicenceNumber.Value,
            Name = staff.Name.Value,
            PhoneNumber = staff.PhoneNumber.Value,
            ContactEmail = staff.ContactEmail.email,
            StaffSpecialization = staff.StaffSpecialization.Value,
            Status = staff.Status.Value,
            Availability = staff.Availability
        };
    }

    // Method to convert to domain model with validation
    public static Staff ToDomain(StaffDataModel staffDataModel)
    {
        return new Staff(
            new LicenceNumber(staffDataModel.LicenceNumber),  // Validation happens in value objects
            new Name(staffDataModel.Name),
            new PhoneNumber(staffDataModel.PhoneNumber),
            new Email(staffDataModel.ContactEmail),
            new StaffSpecialization(staffDataModel.StaffSpecialization),
            new Status(staffDataModel.Status),
            staffDataModel.Availability
        );
    }
    
    public static IEnumerable<Staff> ToDomain(IEnumerable<StaffDataModel> staffsDataModel)
    {
        List<Staff> staffsDomain = new List<Staff>();
        foreach(StaffDataModel staffDataModel in staffsDataModel)
        {
            Staff staffDomain = ToDomain(staffDataModel);
            staffsDomain.Add(staffDomain);
        }
        return staffsDomain.AsEnumerable();
    }
    
}