import { ComponentFixture, TestBed } from '@angular/core/testing';

import { RuComponent } from './ru.component';

describe('RuComponent', () => {
  let component: RuComponent;
  let fixture: ComponentFixture<RuComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [RuComponent]
    })
    .compileComponents();
    
    fixture = TestBed.createComponent(RuComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
