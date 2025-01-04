using G74.Domain.Shared;
using G74.Domain.Value_Objects.Specialization;

namespace G74.Domain.Aggregates.Specialization;

public class Specialization : Entity<Guid>, IAggregateRoot
{
    public Code Code { get; private set; }
    public Designation Designation { get; private set; }
    
    private Specialization()
    {
    }

    public Specialization(Code code, Designation designation) : base(Guid.NewGuid())
    {
        Code = code;
        Designation = designation;
    }
    
    public static Specialization Create(long code, string designation)
    {
        return new Specialization(
            new Code(code),
            new Designation(designation)
        );
    }
}