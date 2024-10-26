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
    
    public IEnumerable<Staff> ToDomain(IEnumerable<StaffDataModel> staffsDataModel)
    {

        List<Staff> staffsDomain = new List<Staff>();

        foreach(StaffDataModel staffDataModel in staffsDataModel)
        {
            Staff staffDomain = ToDomain(staffDataModel);

            staffsDomain.Add(staffDomain);
        }

        return staffsDomain.AsEnumerable();
    }
    
    // Add missing ToDataModel method
    public StaffDataModel ToDataModel(Staff staff)
    {
        return new StaffDataModel(staff);
    }
}