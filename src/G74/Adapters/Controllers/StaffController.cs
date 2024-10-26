using G74.DTO;
using G74.Services;
using Microsoft.AspNetCore.Mvc;

namespace G74.Adapters.Controllers;

[Route("api/[controller]")]
[ApiController]
public class StaffController : ControllerBase
{
    private readonly StaffAppService _staffAppService;
    private readonly StaffToDto _staffToDto;
    
    public StaffController(StaffAppService staffAppService, StaffToDto staffToDto)
    {
        _staffAppService = staffAppService;
        _staffToDto = staffToDto;
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
    
    // POST: api/Staff
    // To protect from overposting attacks, see https://go.microsoft.com/fwlink/?linkid=2123754
    [HttpPost]
    public async Task<ActionResult<StaffDto>> RegisterStaff([FromBody]JsonStaffDto jsonStaffDto)
    {
        try
        {
            StaffDto staffDto = _staffToDto.JsonToDto(jsonStaffDto);
            StaffDto resultStaffDto = await _staffAppService.Add(staffDto);
            
            return CreatedAtAction(
                nameof(GetStaffByLicenseNumber),
                new { licenseNumber = resultStaffDto.LicenseNumber },
                resultStaffDto);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
    }
}