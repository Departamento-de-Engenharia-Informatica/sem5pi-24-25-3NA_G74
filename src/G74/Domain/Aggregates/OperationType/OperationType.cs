
using G74.Domain.Shared;

namespace G74.Domain;

public class OperationType : Entity<OperationTypeId>, IAggregateRoot
{
    
    public OperationName Name { get; private set;}
    public Specialization specialization { get; private set;}
    public Duration estimatedDuration { get; private set;}

    

}