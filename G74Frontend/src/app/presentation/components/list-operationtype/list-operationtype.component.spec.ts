import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ListOperationtypeComponent } from './list-operationtype.component';

describe('ListOperationtypeComponent', () => {
  let component: ListOperationtypeComponent;
  let fixture: ComponentFixture<ListOperationtypeComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ListOperationtypeComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ListOperationtypeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
