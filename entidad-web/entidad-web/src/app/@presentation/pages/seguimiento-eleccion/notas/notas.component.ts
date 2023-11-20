import { Component, Input, OnInit } from '@angular/core';

@Component({
  selector: 'serv-talento-notas',
  templateUrl: './notas.component.html',
  styleUrls: ['./notas.component.scss'],
})
export class NotasComponent implements OnInit {
  @Input() notas = [];
  @Input() nota: string = '0';
  displayedColumns: string[] = ['descripcion', 'nota'];
  constructor() {}

  ngOnInit(): void {}
}
