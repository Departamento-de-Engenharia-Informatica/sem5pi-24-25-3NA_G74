import { ComponentFixture, TestBed } from '@angular/core/testing';

import { DeleteOperationComponent } from './delete-operation.component';

describe('DeleteOperationComponent', () => {
  let component: DeleteOperationComponent;
  let fixture: ComponentFixture<DeleteOperationComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [DeleteOperationComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(DeleteOperationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
