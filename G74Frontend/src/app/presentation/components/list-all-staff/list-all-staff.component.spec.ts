import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ListAllStaffComponent } from './list-all-staff.component';

describe('ListAllStaffComponent', () => {
  let component: ListAllStaffComponent;
  let fixture: ComponentFixture<ListAllStaffComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ListAllStaffComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ListAllStaffComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
