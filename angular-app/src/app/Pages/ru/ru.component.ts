import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { InterpretorService } from '../../Services/interpretor/interpretor.service';
import { HttpClient, HttpClientModule } from '@angular/common/http';

@Component({
  selector: 'app-ru',
  standalone: true,
  imports: [FormsModule, HttpClientModule],
  providers: [InterpretorService],
  templateUrl: './ru.component.html',
  styleUrl: './ru.component.scss'
})
export class RuComponent {
  textarea: string = '';
  output: string = '';

  constructor(private interpretorService: InterpretorService) { }

  public async run() {
    const res = await this.interpretorService.runRuCode(this.textarea);
    this.output = res;
  }
}
