import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ServidoresRepository } from 'src/app/@domain/repository/servidores.repository';
import { ToastService } from '../../@common-components/toast';

@Component({
  selector: 'serv-talento-evaluados',
  templateUrl: './evaluados.component.html',
  styleUrls: ['./evaluados.component.scss']
})
export class EvaluadosComponent implements OnInit {

  constructor(
    private toastService: ToastService,
    private authService: AuthenticationRepository,
    private servidoresRepository: ServidoresRepository,
    private router: Router) { }

  ngOnInit(): void {
    this.servidoresRepository.listarEvaluadores().subscribe(
      (listaEvaluadores) => {
        if (listaEvaluadores.length === 0) {
          this.toastService.showToast(
            'No se encontraron evaluadores',
            'danger'
          );
          return;
        }

        let evaluador = listaEvaluadores.find(x => x.personaId 
          === this.authService.getCurrentUserValue.personaId);
        
          if (!evaluador) {
          this.toastService.showToast(
            'No se encontró el evaluador correspondiente al usuario',
            'danger'
          );
          return;
        }

        evaluador.accedioDirecto = true;
        sessionStorage.setItem('selected_evaluador', JSON.stringify(evaluador));
        this.router.navigate(['pages/participantes/evaluados']);
      },
      (err) => {
        this.toastService.showToast(err.message, 'danger');
      }
    );
  }

}
