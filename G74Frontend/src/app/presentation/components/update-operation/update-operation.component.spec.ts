import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UpdateOperationComponent } from './update-operation.component';

describe('UpdateOperationComponent', () => {
  let component: UpdateOperationComponent;
  let fixture: ComponentFixture<UpdateOperationComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [UpdateOperationComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(UpdateOperationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
