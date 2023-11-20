import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { IParticipanteEvaluador } from 'src/app/@data/model/participante';
import { ServidoresRepository } from 'src/app/@domain/repository/servidores.repository';
import { ToastService } from '../../@common-components/toast';

@Component({
  selector: 'serv-talento-factores-evaluacion',
  templateUrl: './factores-evaluacion.component.html',
  styleUrls: ['./factores-evaluacion.component.scss']
})
export class FactoresEvaluacionComponent implements OnInit {

  constructor(
    private toastService: ToastService,
    private servidoresRepository: ServidoresRepository,
    private router: Router) { }

  ngOnInit(): void {
    let evaluado: IParticipanteEvaluador = {accedioDirecto: true};
    sessionStorage.setItem('selected_participante', JSON.stringify(evaluado));
    this.router.navigate(['pages/participantes/evaluados/metas']);
  }

}
