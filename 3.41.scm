

1. 만약 withdraw나 deposit 함수가 복잡한 함수여서 balance 값을 여러번 읽고 여러번 쓴다면
중간중간 balance 값이 달라지므로, balance 값을 읽는 시간이 언제냐에 따라
balance 값이 달라진다. 따라서 이런 경우에는 withdraw와 deposit 함수가 실행되는 중간에
balance 값을 읽지 못하도록 하는 것이 좋다. -> protected 를하는 것이 좋다.
2. 예제와 같이 withdraw와 deposit 함수가 짧은 경우에는 이 함수들이 실행되는 중간에
balance 값을 읽어도 withdraw 와deposit 함수가 실행이 완료된 후나 실행 직전의
balance 값을 읽게되므로 protected를 할 필요가 없다.

결국 일반적인 상황에서는 상태 값을 읽는 함수도 줄세우기를 하는게 좋지 않을까?
상태를 바꾸는 함수들이 실행되는 중간에 상태값을 알수없는/보장할 수 없는 상황에서 상태값을 읽는게 좋지는 않을것이다.
