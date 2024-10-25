using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Patient;

public class DeletionInformation : IValueObject
{
    
    public bool ToDelete { get;  }
    
    public DateTime ? DateToBeDeleted { get;  }
    
    protected DeletionInformation(){}

    public DeletionInformation(bool toDelete, TimeSpan timeToDeletion)
    {

        ToDelete = toDelete;
        if (ToDelete)
        {
            DateToBeDeleted = DateTime.Now.Add(timeToDeletion);
        }

    }
}