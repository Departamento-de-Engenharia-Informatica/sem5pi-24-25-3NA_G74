using G74.Adapters.Controllers;
using G74.DTO;
using Microsoft.AspNetCore.Mvc;

namespace G74.Infrastructure.Routes;

public class RouteUser
{
    private readonly ControllerUser _userController;
    
    public RouteUser(ControllerUser userController)
    {
        _userController = userController;
    }

    public IActionResult PostRegisterNewBackofficeUser(string json)
    {
        return _userController.RegisterNewUser(json);
    }
}
