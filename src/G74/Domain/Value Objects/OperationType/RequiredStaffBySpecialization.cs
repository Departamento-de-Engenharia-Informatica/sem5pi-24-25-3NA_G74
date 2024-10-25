using System.Linq.Expressions;
using G74.Domain.Shared;

public  class RequiredStaffBySpecialization : IValueObject
{
    public List<SpecializationStaff> SpecializationStaffList {get; private set; }

    public RequiredStaffBySpecialization(List<SpecializationStaff> specializationStaffList)
    {
        this.SpecializationStaffList = specializationStaffList;
    }

    public RequiredStaffBySpecialization(List<string> specializationStaffList)
    {
        this.SpecializationStaffList = new List<SpecializationStaff>();
        foreach (string element in specializationStaffList)
        {
            this.SpecializationStaffList.Add(new SpecializationStaff(element));
        }
    }
}