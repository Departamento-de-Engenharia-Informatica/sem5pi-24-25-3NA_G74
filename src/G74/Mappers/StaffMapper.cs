using G74.Domain.Aggregates.Staff;
using G74.Domain.Factory;
using G74.DTO;

namespace G74.Mappers;

public class StaffMapper
{
    private StaffFactory _staffFactory;

    public StaffMapper(StaffFactory staffFactory)
    {
        _staffFactory = staffFactory;
    }
    
    public Staff ToDomain(StaffDataModel staffDataModel)
    {
        // Changed to instance method using _staffFactory
        return _staffFactory.NewStaff(
            staffDataModel.LicenseNumber,
            staffDataModel.Name,
            staffDataModel.PhoneNumber,
            staffDataModel.ContactEmail,
            staffDataModel.StaffSpecialization);
    }
    
    // Add missing ToDataModel method
    public StaffDataModel ToDataModel(Staff staff)
    {
        return new StaffDataModel(staff);
    }
}