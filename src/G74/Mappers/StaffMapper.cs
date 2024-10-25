using G74.Domain.Aggregates.Staff;
using G74.Domain.Factory;
using G74.DTO;

namespace DefaultNamespace;

public class StaffMapper
{
    private StaffFactory _staffFactory;

    public StaffMapper(StaffFactory staffFactory)
    {
        _staffFactory = staffFactory;
    }
    
    public Staff ToDomain(StaffDataModel staffDataModel)
    {
        Staff staffDomain = _staffFactory.NewStaff(staffDataModel.LicenseNumber, staffDataModel.Name,
            staffDataModel.PhoneNumber, staffDataModel.ContactEmail, staffDataModel.StaffSpecialization);

        //staffDomain.Id = staffDataModel.Id;

        return staffDomain;
    }
}