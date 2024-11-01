using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Patient;

public class DeletionInformation : IValueObject, IEquatable<DeletionInformation>
{
    public bool IsMarkedForDeletion { get; }

    public DateTime? DateToBeDeleted { get; }

    protected DeletionInformation()
    {
    }

    public DeletionInformation(bool isMarkedForDeletion, TimeSpan timeToDeletion)
    {
        IsMarkedForDeletion = isMarkedForDeletion;

        if (IsMarkedForDeletion)
        {
            if (timeToDeletion < TimeSpan.Zero)
                throw new ArgumentException("Time to deletion must be positive when marked for deletion.");

            DateToBeDeleted = DateTime.UtcNow.Add(timeToDeletion);
        }
    }

    public bool Equals(DeletionInformation? other) =>
        other != null &&
        IsMarkedForDeletion == other.IsMarkedForDeletion &&
        Nullable.Equals(DateToBeDeleted?.ToString("yyyy-MM-dd HH:mm:ss"),
            other.DateToBeDeleted?.ToString("yyyy-MM-dd HH:mm:ss"));


    public override bool Equals(object? obj) => obj is DeletionInformation other && Equals(other);

    public override int GetHashCode() => HashCode.Combine(IsMarkedForDeletion, DateToBeDeleted);

    public override string ToString() =>
        IsMarkedForDeletion
            ? $"Marked for deletion on {DateToBeDeleted?.ToString("yyyy-MM-dd HH:mm:ss")}"
            : "Not marked for deletion";
}