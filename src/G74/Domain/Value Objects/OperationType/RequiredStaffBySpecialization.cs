using System.Linq.Expressions;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Staff;

public  class RequiredStaffBySpecialization : IValueObject
{
    public Dictionary<StaffSpecialization, int> SpecializationStaffList {get; private set; }

    public RequiredStaffBySpecialization(Dictionary<StaffSpecialization, int> specializationStaffList)
    {
        this.SpecializationStaffList = specializationStaffList;
    }

    public RequiredStaffBySpecialization(List<string> specializationStaffList, List<int> quantities)
    {
        this.SpecializationStaffList = new Dictionary<StaffSpecialization, int>();
        for (int i = 0; i < specializationStaffList.Count; i++)
        {
            this.SpecializationStaffList.Add(new StaffSpecialization(specializationStaffList[i]), quantities[i]);
        }
        
    }
}