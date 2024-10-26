using G74.DTO;
using G74.Services;
using Microsoft.AspNetCore.Mvc;

namespace G74.Adapters.Controllers;

[Route("api/[controller]")]
[ApiController]
public class StaffController : ControllerBase
{
    private readonly StaffAppService _staffAppService;
    
    public StaffController(StaffAppService staffAppService)
    {
        _staffAppService = staffAppService;
    }
    
    // GET: api/Staff//license/682468
    [HttpGet("license/{licenseNumber}")]
    public async Task<ActionResult<StaffDto>> GetStaffByLicenseNumber(string licenseNumber)
    {
        var staffDTO = await _staffAppService.GetByLicenseNumber(licenseNumber);

        if (staffDTO == null)
        {
            return NotFound();
        }
        return Ok(staffDTO);
    }
    
    // // POST: api/Staff
    // // To protect from overposting attacks, see https://go.microsoft.com/fwlink/?linkid=2123754
    // [HttpPost]
    // public async Task<ActionResult<StaffDto>> RegisterStaff([FromBody]JsonStaffDTO jsonStaffDto)  
    // {
    //     try
    //     {
    //         
    //         
    //         StaffDto resultStaffDTO = await _staffAppService.Add(staffDTO);
    //         
    //         return CreatedAtAction(
    //             nameof(GetStaffByLicenseNumber),
    //             new { licenseNumber = staffDTO.LicenseNumber },
    //             resultStaffDTO);
    //     }
    //     catch (Exception ex)
    //     {
    //         return BadRequest(ex.Message);
    //     }
    // }
}