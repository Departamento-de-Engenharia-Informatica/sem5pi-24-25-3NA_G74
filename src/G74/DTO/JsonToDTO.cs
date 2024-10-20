using G74.Domain.Aggregates.User;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.User;

namespace G74.DTO;

using System;
using System.Text.Json;

public class JsonToDTO
{
    public VoUser CreateVoUser(string json)
    {
        if (string.IsNullOrEmpty(json))
        {
            throw new ArgumentException("JSON data is required.", nameof(json));
        }

        return JsonSerializer.Deserialize<VoUser>(json);
    }

    public string CreateJson(User user)
    {
        if (user == null)
        {
            throw new ArgumentNullException(nameof(user), "User object is required.");
        }
        
        return JsonSerializer.Serialize(user);
    }
}
