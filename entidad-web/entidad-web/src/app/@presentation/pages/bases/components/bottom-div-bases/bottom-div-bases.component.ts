import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import {
  CreacionBaseService,
  ObservacionStepBase,
} from '../../creacion-base/creacion-base.service';
import { AuthenticationRepository } from '../../../../../@domain/repository/authentication.repository';
import { Const } from '../../../../../@data/services/const';

@Component({
  selector: 'serv-talento-bottom-div-bases',
  templateUrl: './bottom-div-bases.component.html',
  styleUrls: ['./bottom-div-bases.component.scss'],
})
export class BottomDivBasesComponent implements OnInit {
  @Input() indexStepper = 0;
  @Input() bodySize = '100vw';
  @Input() numberOfSteps = 4;
  @Input() helper: CreacionBaseService = null;
  @Input() showObservarButton = false;
  @Input() observaciones: ObservacionStepBase[] = [];
  @Input() showEndButton = true;

  @Output() previousStep = new EventEmitter();
  @Output() nextStep = new EventEmitter();
  @Output() createProfile = new EventEmitter();
  @Output() observarClick = new EventEmitter();
  @Output() saveObservaciones = new EventEmitter();

  @Input() createMode = false;

  constructor(private authRepository: AuthenticationRepository) {}

  getValidateActiveSave() {
    if (
      (this.authRepository.isCoordinador() || this.authRepository.isSuperAdminEntidad()) &&
      (this.helper.estadoBase === Const.ETA_BASE_POR_PUBLICAR ||
        this.helper.estadoBase === Const.ETA_BASE_REVISADO)
    ) {
      return this.helper.observacionesNotChange();
    }
    return false;
  }

  ngOnInit(): void {}

  getObservacionesValue() {
    return this.observaciones.filter((obs) => obs.description && !obs.resuelto);
  }
}
