import {
  Component,
  Inject,
  ViewChild,
  ElementRef,
  OnInit,
} from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import {
  FormGroup,
  FormControl,
  Validators,
} from '@angular/forms';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { Subscription, interval } from 'rxjs';
import { AdministratorRepository } from 'src/app/@domain/repository/administrator.repository';

@Component({
  selector: 'serv-talento-modal-email-confirm',
  templateUrl: './modal-email-confirm.component.html',
  styleUrls: ['./modal-email-confirm.component.scss'],
})
export class ModalEmailConfirmComponent implements OnInit {
  @ViewChild('code_1_input', { static: false }) code_1_input: ElementRef;
  @ViewChild('code_2_input', { static: false }) code_2_input: ElementRef;
  @ViewChild('code_3_input', { static: false }) code_3_input: ElementRef;
  @ViewChild('code_4_input', { static: false }) code_4_input: ElementRef;
  @ViewChild('code_5_input', { static: false }) code_5_input: ElementRef;
  @ViewChild('code_6_input', { static: false }) code_6_input: ElementRef;

  subscription: Subscription;

  time: number = 1;
  dateNow = new Date();
  dDay = new Date(this.dateNow.getTime() + this.time * 60000);

  milliSecondsInASecond = 1000;
  hoursInADay = 24;
  minutesInAnHour = 60;
  SecondsInAMinute = 60;

  timeDifference;
  secondsToDday;
  minutesToDday;
  hoursToDday;
  daysToDday;

  form: FormGroup;
  status: string = 'basic';
  constructor(
    private dialogRef: MatDialogRef<ModalEmailConfirmComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
    private toast: ToastService,
    private administratorRepository: AdministratorRepository
  ) {}
  ngOnInit() {
    this.form = new FormGroup({
      code_1: new FormControl('', [
        Validators.required,
        Validators.pattern(/^-?(0|[1-9]\d*)?$/),
      ]),
      code_2: new FormControl('', [
        Validators.required,
        Validators.pattern(/^-?(0|[1-9]\d*)?$/),
      ]),
      code_3: new FormControl('', [
        Validators.required,
        Validators.pattern(/^-?(0|[1-9]\d*)?$/),
      ]),
      code_4: new FormControl('', [
        Validators.required,
        Validators.pattern(/^-?(0|[1-9]\d*)?$/),
      ]),
      code_5: new FormControl('', [
        Validators.required,
        Validators.pattern(/^-?(0|[1-9]\d*)?$/),
      ]),
      code_6: new FormControl('', [
        Validators.required,
        Validators.pattern(/^-?(0|[1-9]\d*)?$/),
      ]),
    });

    this.initTimer ();
  }

  initTimer () {
    this.subscription = interval(1000).subscribe((x) => {
      this.getTimeDifference();
    });
  }

  OnDestroy() {
    this.subscription.unsubscribe();
  }

  getTimeDifference() {
    this.timeDifference = this.dDay.getTime() - new Date().getTime();
    this.allocateTimeUnits(this.timeDifference);
  }

  allocateTimeUnits(timeDifference) {
    this.secondsToDday = Math.floor(
      (timeDifference / this.milliSecondsInASecond) % this.SecondsInAMinute
    );
    this.minutesToDday = Math.floor(
      (timeDifference / (this.milliSecondsInASecond * this.minutesInAnHour)) %
        this.SecondsInAMinute
    );
    this.hoursToDday = Math.floor(
      (timeDifference /
        (this.milliSecondsInASecond *
          this.minutesInAnHour *
          this.SecondsInAMinute)) %
        this.hoursInADay
    );
    this.daysToDday = Math.floor(
      timeDifference /
        (this.milliSecondsInASecond *
          this.minutesInAnHour *
          this.SecondsInAMinute *
          this.hoursInADay)
    );
  }

  validar() {
    if (this.timeDifference <= 0) {
      this.administratorRepository
        .verifyEmail(this.data.correo)
        .toPromise()
        .then((code: any) => {
          this.subscription.unsubscribe();
          this.data.code = code;
          this.dateNow = new Date();
          this.dDay = new Date(this.dateNow.getTime() + this.time * 60000);

          this.initTimer ();
        })
        .catch((error: any) => {});
    } else {
      if (this.form.invalid) {
        this.toast.showToast(
          'Debe completar todos los campos que son obligatorios',
          'danger'
        );
        return;
      }

      if (this.data.code === this.getCode()) {
        this.dialogRef.close(true);
      } else {
        this.status = 'danger';
        this.form.reset();
        this.toast.showToast('Codigo de verificación invalido', 'danger');
        setTimeout(() => {
          this.status = 'basic';
          this.code_1_input.nativeElement.select();
        }, 1000);
      }
    }
  }

  getCode() {
    let returned: string = '';

    returned =
      this.form.getRawValue().code_1 +
      this.form.getRawValue().code_2 +
      this.form.getRawValue().code_3 +
      this.form.getRawValue().code_4 +
      this.form.getRawValue().code_5 +
      this.form.getRawValue().code_6;

    return returned;
  }

  change_code_input(event: any, next_input: any) {
    if (event) {
      if (next_input === 'code_2_input') {
        this.code_2_input.nativeElement.select();
      } else if (next_input === 'code_3_input') {
        this.code_3_input.nativeElement.select();
      } else if (next_input === 'code_4_input') {
        this.code_4_input.nativeElement.select();
      } else if (next_input === 'code_5_input') {
        this.code_5_input.nativeElement.select();
      } else if (next_input === 'code_6_input') {
        this.code_6_input.nativeElement.select();
      }
    }
  }
}
