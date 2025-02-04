import { ComponentFixture, TestBed } from '@angular/core/testing';

import { LispComponent } from './lisp.component';

describe('LispComponent', () => {
  let component: LispComponent;
  let fixture: ComponentFixture<LispComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [LispComponent]
    })
    .compileComponents();
    
    fixture = TestBed.createComponent(LispComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
