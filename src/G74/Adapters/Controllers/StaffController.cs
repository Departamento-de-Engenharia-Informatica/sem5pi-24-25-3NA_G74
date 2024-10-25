using Microsoft.AspNetCore.Mvc;
using G74.Services;
using G74.DTO;

namespace DefaultNamespace;

[Route("api/[controller]")]
[ApiController]
public class StaffController : ControllerBase
{
    private readonly StaffService _staffService;
    
    public StaffController(StaffService staffService)
    {
        _staffService = staffService;
    }
    
    // GET: api/Staff//license/682468
    [HttpGet("license/{licenseNumber}")]
    public async Task<ActionResult<StaffDto>> GetStaffByLicenseNumber(string licenseNumber)
    {
        var staffDTO = await _staffService.GetByLicenseNumber(licenseNumber);

        if (staffDTO == null)
        {
            return NotFound();
        }
        return Ok(staffDTO);
    }
    
    // POST: api/Staff
    // To protect from overposting attacks, see https://go.microsoft.com/fwlink/?linkid=2123754
    [HttpPost]
    public async Task<ActionResult<StaffDto>> PostStaff(StaffDto staffDTO)  
    {
        try
        {
            StaffDto resultStaffDTO = await _staffService.Add(staffDTO);
            
            return CreatedAtAction(
                nameof(GetStaffByLicenseNumber),
                new { licenseNumber = staffDTO.LicenseNumber },
                resultStaffDTO);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
    }
}