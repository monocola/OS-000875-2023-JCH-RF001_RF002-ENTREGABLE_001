import { Const } from 'src/app/@data/services/const';
import { Component, OnInit, Inject } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { EvaluacionCurricularRepository } from 'src/app/@domain/repository/evaluacion-curricular.repository';

@Component({
  selector: 'serv-talento-modal-ver-documento',
  templateUrl: './modal-ver-documento.component.html',
  styleUrls: ['./modal-ver-documento.component.scss'],
})
export class ModalVerDocumentoComponent implements OnInit {
  constructor(
    private evaluacionCurricularService: EvaluacionCurricularRepository,
    private matDialogRef: MatDialogRef<ModalVerDocumentoComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngOnInit(): void {}

  onNoClick(ans: boolean = false) {
    this.matDialogRef.close(ans);
  }

  verDocumento(item: any) {
    // const request: any = {
    //   trace: {
    //     traceId: 'string',
    //   },
    //   payload: {
    //     rutaArchivo: item.urlDocumento,
    //   },
    // };
    // window.location.href = Const.API_FILE_SERVER + item.urlDocumento;
    window.open(Const.API_FILE_SERVER + item.urlDocumento, "_blank");

    // this.evaluacionCurricularService
    //   .documentoSustento(request)
    //   .toPromise()
    //   .then((res: any) => {
    //     window.open(res.urlFullArchivo, '_blank');
    //   });
  }
}
