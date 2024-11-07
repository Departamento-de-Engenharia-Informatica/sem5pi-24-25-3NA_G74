using System.ComponentModel.DataAnnotations;
namespace G74.Domain.Shared
{
    /// <summary>
    /// Base class for entities.
    /// </summary>
    public abstract class Entity<TId> : IEquatable<Entity<TId>>
    {
        [Key]
        public TId Id { get; protected set; }

        protected Entity(TId id)
        {
            Id = id ?? throw new ArgumentNullException(nameof(id));
        }

        protected Entity()
        {
        }

        public override bool Equals(object? obj) => Equals(obj as Entity<TId>);

        public bool Equals(Entity<TId>? other)
        {
            if (other is null)
                return false;

            return GetType() == other.GetType() && Id.Equals(other.Id);
        }

        public override int GetHashCode() => Id.GetHashCode();

        public static bool operator ==(Entity<TId>? left, Entity<TId>? right) =>
            Equals(left, right);

        public static bool operator !=(Entity<TId>? left, Entity<TId>? right) =>
            !Equals(left, right);
    }

    public abstract class EntityId : IEquatable<EntityId>
    {
        protected object ObjValue { get; }

        public string Value => ObjValue is string str ? str : AsString();

        protected EntityId(object value)
        {
            ObjValue = value ?? throw new ArgumentNullException(nameof(value));
        }

        protected abstract object CreateFromString(string text);
        
        public abstract string AsString();

        public override bool Equals(object? obj) => Equals(obj as EntityId);

        public bool Equals(EntityId? other)
        {
            if (other is null) return false;
            return GetType() == other.GetType() && Value == other.Value;
        }

        public override int GetHashCode() => Value.GetHashCode();

        public static bool operator ==(EntityId? left, EntityId? right) =>
            Equals(left, right);

        public static bool operator !=(EntityId? left, EntityId? right) =>
            !Equals(left, right);
    }
}