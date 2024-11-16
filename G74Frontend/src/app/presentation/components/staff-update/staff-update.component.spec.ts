import { ComponentFixture, TestBed } from '@angular/core/testing';

import { StaffUpdateComponent } from './staff-update.component';

describe('StaffUpdateComponent', () => {
  let component: StaffUpdateComponent;
  let fixture: ComponentFixture<StaffUpdateComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [StaffUpdateComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(StaffUpdateComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
