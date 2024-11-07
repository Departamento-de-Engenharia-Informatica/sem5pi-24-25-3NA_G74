import { ComponentFixture, TestBed } from '@angular/core/testing';

import { RegisterOperationComponent } from './register-operation.component';

describe('RegisterOperationComponent', () => {
  let component: RegisterOperationComponent;
  let fixture: ComponentFixture<RegisterOperationComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [RegisterOperationComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(RegisterOperationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
